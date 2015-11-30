#lang racket/base

(require racket/class
         racket/draw)



;; (provide (prefix-out ebuffer: (combine-out (suffixed-as interface mixin class
;;                                                         #:from (all-defined-out))
;;                                            current-node?
;;                                            indicator?)))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(eval '(new object%)
      ns)

(is-a?/c bitmap-dc%)



;; (thread (lambda ()
;;           (for ([i 10])
;;             (sleep 2)
;;             (printf "thread 1\n"))))


;; (thread (lambda ()
;;           (for ([i 20])
;;             (sleep 1)
;;             (printf "thread 2\n"))))

;; #lang racket/base

;; ; One way to define a logger
;; (define lg (make-logger 'my-logger))
;; ; Define a receiver for this logger, along with a log level
;; (define rc (make-log-receiver lg 'error)) ; also try with 'debug

;; ; Another way to define a logger, with additional forms
;; (define-logger lg2)
;; (define rc2 (make-log-receiver lg2-logger 'debug))

;; ; Listen for events on the two log-receivers
;; (void 
;;  (thread 
;;   (λ()(let loop () 
;;         (define v (sync rc rc2))
;;         (printf "[~a] ~a\n" (vector-ref v 0) (vector-ref v 1)) 
;;         (loop)))))

;; ; Set the current logger 
;; (current-logger lg)

;; ; Log messages for the current logger
;; (log-error "Exterminate!")
;; (log-fatal "Exterminate! Exterminate!")
;; (log-debug "What's the red button for?")

;; ; Log messages for lg2 specifically
;; (log-lg2-info "We're on a mission from God.")
;; (sleep 0.4)

;; (foldl (λ (x res)
;;           (printf "~a ~a\n" x res)
;;           (+ x res))
;;        0
;;        '(1 2 3 4))

;; (define (pp v p [d 0])
;;   (print "asd"))

;; (parameterize [(global-port-print-handler pp)]
;;   (print "de"))



;; [vector-copy (All (a) (->* ((Vectorof a)) (Integer Integer) (Vectorof a)))]

;; (vector-append (vector 1)
;;                (vector 2))



;; (require racket/draw)


;; (define indicator<%>
;;   (interface ()

    
;;     [render! (->m (is-a?/c bitmap-dc%) string? void?)]

    
;;     ))


;; (define-namespace-anchor anchor)
;; (define ns (namespace-anchor->namespace anchor))

;; (eval '(new object%)
;;       ns)


