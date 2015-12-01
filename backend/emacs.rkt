#lang racket/base

(require racket/class
         racket/contract

         "buffer.rkt"
         "../constants.rkt"

         ss-rpc-server
         
         (for-syntax racket/base
                     racket/syntax))


(provide read-string-param
         emacs
         genuine-emacs)


(define emulator-ht #hash())

(define-syntax (define-emulation-proc stx)
  (syntax-case stx ()
    [(_ (name . params) body ...)
     (with-syntax ([prefixed-name (format-id #'name "el-~a" (syntax-e #'name))])
       #'(begin (define (prefixed-name . params)
                  body ...)
                (set! emulator-ht (hash-set emulator-ht 'name prefixed-name))))]))




(define yes-or-no-p-param (make-parameter 't))
(define read-string-param (make-parameter "read string value"))

(define-emulation-proc (yes-or-no-p message)
  (yes-or-no-p-param))

(define-emulation-proc (read-string message)
  (read-string-param))




(define emacs<%>
  (interface ()
    [deferred-call (->*m ((or/c symbol? list?)) #:rest (listof any/c) any/c)]
    [direct-call   (->*m ((or/c symbol? list?)) #:rest (listof any/c) any/c)]
    [buffer% (->m (implementation?/c buffer<%>))]))




(define emulated-emacs%
  (class* object% (emacs<%>)
    (super-new)

    (field [project-buffer (new emulated-buffer% [name const:project-tree-buffer-name])])

    (define/public (deferred-call proc . args) (void))
    
    (define/public (direct-call proc . args)
      (cond
       [(hash-has-key? emulator-ht proc)
        (apply (hash-ref emulator-ht proc) args)]
       [else (printf "direct-call of unimplemented emulation procedure ~a\n" proc)]))

    (define/public (buffer%) emulated-buffer%)

    
    
    ))

(define genuine-emacs%
  (class* object% (emacs<%>)
    (super-new)

    (field [project-buffer (new emacs-buffer% [name const:project-tree-buffer-name])])
    
    (define/public (deferred-call proc . args)
      (apply call! proc args))

    (define/public (direct-call proc . args)
      (apply call proc args))

    (define/public (buffer%) emacs-buffer%)

    ))


(define emulated-emacs (new emulated-emacs%))
(define genuine-emacs (new genuine-emacs%))

(define emacs (make-parameter emulated-emacs))
