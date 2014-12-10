#lang racket/base

(require racket/contract
         racket/class
         racket/match
         racket/function

         "../base.rkt"
         "../file.rkt")

(provide (all-defined-out))

(define-struct/contract annotation ([node (is-a?/c base:node<%>)]) #:transparent)
(define-struct/contract (call-annotation annotation) ([method symbol?]) #:transparent)
(define-struct/contract (value-annotation annotation) ([value any/c]) #:transparent )

(define symboled-node<%>
  (interface (base:node<%>)
    init-sym!))


(define symboled-node-mixin
  (mixin (base:node<%>) (symboled-node<%>)
    (inspect #f)
    (super-new)
    (field [sym 'uninitialized])
    
    (define/public (init-sym! v)
      (set! sym v))))



(define symbol-inspected-node-mixin
  (mixin (symboled-node<%>) ()
    (inspect #f)
    (super-new)

    (define/override (repr-fields)
      (cons 'sym (super repr-fields)))

    (define/override (equality-fields)
      (cons 'sym (super equality-fields)))
    
    ))


(define file-name-refine-mixin
  (mixin (file:node<%> symboled-node<%>) ()
    (inspect #f)
    (super-new)

    (inherit init-name!)

    (define/override (init-sym! v)
      (super init-sym! v)
      (init-name! (string->path (symbol->string v))))
        
    ))



(define (make-os-file-tree path tree)
  (match tree
    [(cons x xs)
     (match x
       [(cons sx sxs)
        (define new-path (build-path path (symbol->string sx)))
        (make-directory new-path)
        (make-os-file-tree new-path sxs)]

       [_
        (define new-path (build-path path (symbol->string x)))
        (with-output-to-file new-path void)])
     (make-os-file-tree path xs)]
    
    [_ (void)]))
