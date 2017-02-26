#lang racket/base

(require racket/contract
         racket/match
         racket/function
         racket/vector
         racket/format
         racket/list
         racket/set
         racket/string
         
         ss/racket/class
         ss/racket/syntax

         ss-rpc-server
         
         "buffer-string.rkt"

         (for-syntax racket/base
                     
                     syntax/parse
                     syntax/stx))

;; buffer-string

(provide buffer<%>
         emulated-buffer%
         emacs-buffer%
        
         
         make-buffer-string)


(define-logger buffer)




(define buffer<%>
  (interface ()
    [substring-content (->m natural-number/c natural-number/c buffer-string?)]
    
    [insert (->*m (buffer-string?)
                  (#:point (or/c natural-number/c #f))
                  void?)]
    [delete-region (->m natural-number/c natural-number/c void?)]
    [put-text-property (->m natural-number/c natural-number/c symbol? any/c void?)]
    [set-header! (->m buffer-string? void?)]
    [goto-char! (->m natural-number/c void?)]
    [clear-buffer! (->m void?)]
    [switch-to-source-code-buffer! (->m void)]
    [kill! (->m void?)]
    [get-content (->m string?)]
    call-in-buffer
    
    ))



(define-syntax (make-buffer-string stx)
  (define (make-constructor string-stx)
    (syntax-parse string-stx
      [(x:expr (~seq prop:expr value:expr) ...)
       #'(bs-make x (list (cons prop value) ...))]
      
      [x:id #'x]
      [x:expr #'(bs-make x '())]))
  
  (syntax-parse stx
    [(_ x ...+)
     (with-syntax ([(constructor ...) (stx-map make-constructor #'(x ...))])
       #'(bs-append constructor ...))]))


;; todo make serializable
(define abstract-buffer%
  (class* object% (buffer<%>)
    (super-new)
    (init-field name)

    (abstract insert
              delete-region
              call-in-buffer
              substring-content
              put-text-property
              goto-char!
              clear-buffer!
              set-header!
              switch-to-source-code-buffer!
              kill!
              get-content)
    ))


(define emulated-buffer%
  (class abstract-buffer%
    (super-new)
    
    (field [point 1]
           [content (buffer-string "" #())]
           [header #f])

    (define/override (clear-buffer!)
      (set! point 1)
      (set! content (buffer-string "" #())))
    
    (define/override (insert bs #:point [pnt point])
      (set! content (bs-append (bs-substring content 0 (sub1 pnt))
                               bs
                               (bs-substring content (sub1 pnt))))
      (set! point (+ pnt (bs-length bs))))
      


    

    (define/override (call-in-buffer proc . args)
      (printf "\nWARNING, registered attempt to call unimplemented buffer procedure `~a'\n" proc))
    
    (define/override (delete-region from to)
      (set! content (bs-append (bs-substring content 0 (sub1 from))
                               (bs-substring content (sub1 to))))

      (set! point (- point (- to from))))

    (define/override (put-text-property from to prop value)
      (define to-propertize (bs-substring content (sub1 from) (sub1 to)))
      
      (set! content (bs-append (bs-substring content 0 (sub1 from))
                               (struct-copy buffer-string to-propertize
                                     [properties (vector-map (λ (e) (hash-set e prop value))
                                                             (buffer-string-properties to-propertize))])
                               (bs-substring content (sub1 to)))))
    
    



    (define/override (substring-content from [to (add1 (send content bs-length))])
      (bs-substring content (sub1 from) (sub1 to)))

    (define/override (goto-char! pos) (void)
      ;; (set! point (sub1 pos))
      )
    

    (define/override (set-header! v) (set! header v))

    (define/override (switch-to-source-code-buffer!) (void))
    (define/override (kill!) (set! content 'killed))
    (define/override (get-content) content)
    
    ))



(define emacs-buffer%
  (class abstract-buffer%
    (super-new)

    (inherit-field name)
    
    (define/override (call-in-buffer proc . args)      
      (call! 'call-in-buffer name proc args))
    
    
    (define/override (insert buffer-string #:point [point #f])
      ;; todo split it
      (cond
       [point (call! 'insert-to-at-point buffer-string name point)]
       [else (call! 'insert-to buffer-string name)]))

    (define/override (set-header! buffer-string)
      (call! 'set-buffer-header name buffer-string))

    
    (define/override (delete-region from to)
      (call-in-buffer 'delete-region from to))

    (define/override (put-text-property from to face value)
      (call-in-buffer 'put-text-property from to face value))

    
    (define/override (substring-content from to)
      (error 'not-implemented))

    (define/override (goto-char! pos)
      (call-in-buffer `(lambda () (set-window-point (get-buffer-window ,name) ,pos))))

    (define/override (clear-buffer!)
      ;; todo try erase-buffer
      (call-in-buffer `(lambda () (delete-region 1 (point-max)))))

    (define/override (switch-to-source-code-buffer!)
      ;; (call! 'switch-to-buffer name)
      (call! 'pt:set-source-code-buffer name))
    
    ;; (define/override (switch-to-buffer!)
    ;;   ;; (call! 'switch-to-buffer name)
    ;;   (call! 'pt:set-source-code-buffer name)
    ;;   )
    
    (define/override (kill!) (call! 'kill-buffer name))
    (define/override (get-content) (call 'buffer-string-no-properties name))
    
    ))






