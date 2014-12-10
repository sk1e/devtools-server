#lang racket/base

(require racket/class
         racket/contract

         "../sg-epc.rkt"

         (for-syntax racket/base
                     racket/syntax))


(provide backend-container?
         backend-container%

         backend-specifier<%>
         emulated-backend-mixin
         emacs-backend-mixin

         read-string-res)


(define (backend-container? v)  (is-a? v backend-container%))

(define backend-specifier<%>
  (interface ()
    [get-backend (->m backend-container? any)]
    [deferred-call (->*m (symbol?) #:rest (listof any/c) any/c)]
    [direct-call   (->*m (symbol?) #:rest (listof any/c) any/c)]
    ))




(define emulator-ht #hash())

(define-syntax (define-emulation-proc stx)
  (syntax-case stx ()
    [(_ (name . params) body ...)
     (with-syntax ([prefixed-name (format-id #'name "el-~a" (syntax-e #'name))])
       #'(begin (define (prefixed-name . params)
                  body ...)
                (set! emulator-ht (hash-set emulator-ht 'name prefixed-name))))]))




(define yes-or-no-p-res (make-parameter 't))
(define read-string-res (make-parameter "read string value"))

(define-emulation-proc (yes-or-no-p message)
  (yes-or-no-p-res))

(define-emulation-proc (read-string message)
  (read-string-res))


(define emulated-backend-mixin
  (mixin () (backend-specifier<%>)
    (inspect #f)
    (super-new)

    (define/public (get-backend obj)
      (send obj get-backend 'emulated))

    (define/public (deferred-call proc . args) (void))
    
    (define/public (direct-call proc . args)
      (cond
       [(hash-has-key? emulator-ht proc)
        (apply (hash-ref emulator-ht proc) args)]
       [else (printf "direct-call of unimplemented emulator ~a\n" proc)]))

    

    
    ))

(define emacs-backend-mixin
  (mixin () (backend-specifier<%>)
    (inspect #f)
    (super-new)
    
    (define/public (get-backend obj)
      (send obj get-backend 'emacs))


    (define/public (deferred-call proc . args)
      (apply el-deferred-call proc args))

    (define/public (direct-call proc . args)
      (apply el-direct-call proc args))

    ))


(define backend-container%
  (class object%
    (super-new)
    (init-field emulated emacs)
    
    (define/public (get-backend type)
      (case type
        [(emacs)    emacs]
        [(emulated) emulated]))
    
    ))
