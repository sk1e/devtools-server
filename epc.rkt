#lang racket/base


(require (for-syntax racket/syntax
                     racket/base)

         racket/class
         racket/tcp
         racket/contract
         racket/match
         racket/function
         racket/format
         racket/string
         racket/async-channel
         srfi/19)


(provide define-epc-method
         register-epc-method
         server%
         epc-logger)


(define-struct (exn:fail:remote-call exn:fail) ())

(define-struct (exn:fail:remote-call:application exn:fail:remote-call) ())
(define-struct (exn:fail:remote-call:epc         exn:fail:remote-call) ())

;(define epc-logger (make-logger 'epc))
(define-logger epc)

(define receiver (make-log-receiver epc-logger 'debug))

;; (define child-looger (make-logger 'qwe epc-logger))

(current-logger epc-logger)


(define tt
  (thread 
   (λ ()
      (let loop ()
        (define v (sync receiver))
        
        (call-with-output-file "/home/god/repc-log.txt"
          (λ (out)
             (define header (format "[~a ~a]"
                                    (vector-ref v 0)
                                    (date->string (current-date) "~H:~M:~S")))
             (displayln (format "~a ~a"
                                header
                                (string-join (string-split (vector-ref v 1) "\n")
                                             (string-append "\n" (make-string (add1 (string-length header))
                                                                              #\space))))
                        out))
          #:mode 'text #:exists 'append)
        
        
        (loop)))))


(define-syntax (define-procedure-set stx)
  
  (syntax-case stx ()
    [(_ name) (let ([format-name (λ (#:suffix [sfx ""] #:prefix [pfx ""])
                                    (format-id #'name "~a~a~a" pfx (syntax-e #'name) sfx))])
                
                (with-syntax ([ht-id (format-name #:suffix "-ht")]
                              [predicate-id (format-name #:suffix "-symbol?")]
                              [registerer-id (format-name #:prefix "register-")]; #:suffix "-proc")]
                              [getter-id (format-name #:prefix "get-")]; #:suffix "-proc")]
                              [definer-id (format-name #:prefix "define-")]
                              [contracted-definer-id (format-name #:prefix "define-" #:suffix "/contract")])
                  
                  #'(begin (define ht-id (make-hasheq))
                           (define (predicate-id v) (hash-has-key? ht-id v))
                           
                           (define-syntax-rule (registerer-id proc)
                             (hash-set! ht-id 'proc proc))
                           
                           (define (getter-id proc-symbol)
                             (hash-ref ht-id proc-symbol))
                           
                           (define-syntax-rule (definer-id (id . rest) body (... ...))
                             (begin (define (id . rest) body (... ...))
                                    (registerer-id id)))

                           (define-syntax-rule (contracted-definer-id (id . rest) contract body (... ...))
                             (begin (define/contract (id . rest) contract body (... ...))
                                    (registerer-id id)))
                               
                              
                     
                     )))]))
  

(define-procedure-set epc-method)
(define-procedure-set question-handler)
(define-procedure-set answer-handler)

;; (define-epc-method (echo . args)
;;   args)

;; (define-epc-method (slow-echo . args)
;;   ;; (sleep 10)
;;   args)


;; (define-epc-method (remote-method)
;;   '(message "privet"))



(define default+nil-readtable (make-readtable (current-readtable) #\n 'non-terminating-macro
                                              (λ (c in . _)
                                                 ;; tofix satisfies [any]il
                                                 (define sym (read in))
                                                 (if (eq? sym 'il)
                                                     null
                                                     (string->symbol (format "n~a" sym))))))






(define server%
  (class object%
    (super-new)

    (define-values (port-in port-out) (values 'uninitialized 'uninitialized))
    (define income-pc-channel (make-async-channel))

    (define/public (serve)
      (define listener (tcp-listen 0 4 #f "localhost"))
      (define-values (_ port __ ___) (tcp-addresses listener #t))
      (displayln port)
      (set!-values (port-in port-out) (tcp-accept listener))
      (log-epc-info "connection accepted")
      
      (define receiver-thread (thread recieve))
      
      (let loop ()
        ((sync income-pc-channel))
        (unless (thread-dead? receiver-thread)
          (loop))))


    (define (recieve)
      (with-handlers ([exn:fail? (λ (e) (log-epc-fatal (exn-message e)))])
        (let loop ()
          (sync port-in)
          (log-epc-debug "got input")
          
          (define len-string (read-string 6 port-in))
          
          (unless (eof-object? len-string)
            (let* ([len (string->number len-string 16)]
                   [message (read-string len port-in)])
              
              (log-epc-debug "recieved: ~a" message)
              
              (when (< len (string-length message))
                (error 'bad-payload-length-value))
              
              (match (parameterize ([current-readtable default+nil-readtable])
                       (read (open-input-string message)))
                
                [(list (? question-handler-symbol? handler) uid body ...)
                 (with-handlers ([exn:fail? (λ (e) (log-epc-error (exn-message e))
                                               (send-list (list 'epc-error uid (exn-message e))))])
                   (apply (get-question-handler handler) uid body))]
                
                [(cons (? answer-handler-symbol? handler) body) (apply (get-answer-handler handler) body)]
                
                [_ (log-epc-error "bad message")]
                
                )
              
              (loop))))
        (log-epc-info "recieved eof, closing ports")
        
        (close-input-port port-in)
        (close-output-port port-out)))
  
  
  
  (define (send-list lst)
    (log-epc-debug "send encode")
    (define content (string->bytes/utf-8 (~s lst)))
    (define message (bytes-append (string->bytes/utf-8 (~r (bytes-length content) #:min-width 6 #:pad-string "0" #:base 16))
                                   content))
    
    (log-epc-debug "sending ~a" message)
    (display message port-out)
    (flush-output port-out))


  
    (define uid 1)
    (define (uid? v) (eq? v uid))
    
    (define active-call-session-ch (make-channel))
    

    (define/public (call-remote-procedure proc args)
      (send-list (list 'call uid proc args))
      (define answer (sync active-call-session-ch))
      (set! uid (add1 uid))
      (if (exn:fail? answer)
          (raise answer)
          answer))


    (define call-list '())
    (define/public (append-call proc args)
      (set! call-list (cons (list proc args) call-list)))
    
    
    (define-question-handler/contract (call uid method-symbol args)
      (-> natural-number/c epc-method-symbol? list? void?)
      (async-channel-put income-pc-channel
                         (λ () (send-list (with-handlers ([exn:fail? (λ (e)
                                                                        (log-epc-error (exn-message e))
                                                                        (list 'return-error uid (exn-message e)))])
                                            ;; (list 'return uid (apply (get-epc-method method-symbol) args))
                                            (define method-result (apply (get-epc-method method-symbol) args))
                                            (define elisp-calls (reverse call-list))
                                            (set! call-list '())
                                            (list 'return uid  (cond
                                                                [(null? elisp-calls) (cond
                                                                                      [(void? method-result) '()]
                                                                                      [else method-result])]
                                                                [else elisp-calls])))))))
    
    
    
    (define-answer-handler/contract (return uid value)
      (-> uid? any/c void?)
      (channel-put active-call-session-ch value))
    

    (define-answer-handler/contract (return-error uid error-message)
      (-> natural-number/c any/c void?)
      (channel-put active-call-session-ch (exn:fail:remote-call:application (format "application error [~a]:\n~a" uid error-message)
                                                                            (current-continuation-marks))))
    

    (define-answer-handler/contract (epc-error uid text)
      (-> natural-number/c string? void?)
      (channel-put active-call-session-ch (exn:fail:remote-call:epc (format "EPC error [~a]:\n~a" uid text)
                                                                    (current-continuation-marks))))
    
    ))
