#lang racket/base

(require racket/class
         racket/contract

         ;"specifier.rkt"
         "../sg-epc.rkt")

(provide (all-defined-out))


(define emacs<%>
  (interface ()
    [deferred-call (->*m (symbol?) #:rest (listof any/c) any/c)]
    [direct-call   (->*m (symbol?) #:rest (listof any/c) any/c)]
    ))



(define emacs%
  (class* object% (emacs<%>)
    (super-new)

    (define/public (deferred-call proc . args)
      (apply el-deferred-call proc args))

    (define/public (direct-call proc . args)
      (apply el-direct-call proc args))

   

    ))



(define emulated-emacs%
  (class* object% (emacs<%>)
    (super-new)

    

    (define/public (deferred-call proc . args)
      (printf "\nemulated deferred-call of ~a\n" proc))
    
    (define/public (direct-call proc . args)
      (printf "\nemulated direct-call of ~a\n" proc))

    

    ))

(define emacs (new emacs%))
(define emulated-emacs (new emulated-emacs%))

;; (define emacs (new backend-container%
;;                    [emulated (new emulated-emacs%)]
;;                    [emacs (new emacs%)]))
