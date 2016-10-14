#lang racket/base

(require racket/unit
         racket/contract
         racket/match
         racket/function
         
         ss/racket/class
         
         "descendant-sig.rkt"
         "ancestor-sig.rkt"
         
         "descendant-unit.rkt"
         "ancestor-unit.rkt"         
         
         "runnable.rkt"
         "../../constants.rkt")

(provide test<%>
         test-mixin
         module-test%)


(define test<%>
  (interface (runnable<%>)
    [on-test-result! (->m natural-number/c void?)]
    [toggle-test! (->m void?)]))

(define test-mixin
  (mixin (runnable<%>) (test<%>)
    (super-new)
    (inherit-field name)
    
    (field [tested-module 'uninitialized])

    (inherit node-identifier
             absolute-path
             run!)
    

    (define (deinit-test)
      (set-field! test-module tested-module #f)
      (send+ tested-module (test-indicator) (switch-off!)))
    
    (define/override (remove-from-tree!)
      (super remove-from-tree!)
      (deinit-test))
    
    (define/override (delete!)
      (super delete!)
      (deinit-test))

    
    (define/override (on-error-exit-status!)
      (send+ tested-module
             (test-indicator)
             (change-color! const:indicator-test-warning-color)))

    (define/override (run-module-at-foreground!)
      (run! 'pt:execute-test-at-foreground))

    
    (define/public (on-test-result! failures-num)
      (send+ tested-module
             (test-indicator)
             (change-color! (match failures-num
                              [0 const:indicator-test-base-color]
                              [(? (curryr > 0) _) const:indicator-test-warning-color]
                              [_ (error 'unexpected-failures-number "~a" failures-num)]))))

    (define/public (toggle-test!)
      (send tested-module select-as-new!))    
    
    ))

(define-inspected-class module-test% (class-from-mixins runnable-final-sum test))

