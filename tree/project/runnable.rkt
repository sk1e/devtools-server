#lang racket/base

(require racket/unit
         racket/contract
         racket/match
         
         ss/racket/class
         
         "descendant-sig.rkt"
         "ancestor-sig.rkt"
         
         "descendant-unit.rkt"
         "ancestor-unit.rkt"         

         "leaf.rkt"
         "../file.rkt"
         "../ebuffer.rkt"
         "../../constants.rkt"
         "../../backend/emacs.rkt")

(provide runnable<%>
         runnable-final-sum-mixin)


(define runnable<%>
  (interface (leaf<%>)
    [run! (->m symbol? void?)]
    [run-start! (->m symbol? void?)]
    [interrupt-execution! (->m void?)]
    [mark-as-running! (->m void?)]
    [mark-as-not-running! (->m void?)]

    [on-exit-status! (->m integer? void?)]
    [on-normal-exit-status! (->m void?)]
    [on-error-exit-status! (->m void?)]

    [run-module-at-foreground! (->m void?)]
    ))

    
    

(define runnable-mixin
  (mixin (leaf<%>) (runnable<%>)
    (super-new)
    
    (inherit-field name)
    
    (inherit absolute-path
             buffer-name
             root
             put-extra-ftf-prop!
             remove-extra-ftf-prop!)

    (abstract run-module-at-foreground!)

    
    (define/public (run-start! elisp-executor)
      (define exec-buffer-name (format "*exec ~a*" (buffer-name)))
      (send (emacs) deferred-call 'pt:init-exec-buffer exec-buffer-name)
      (send (emacs) deferred-call elisp-executor exec-buffer-name (path->string (absolute-path)))
      (mark-as-running!))

    
    (define/public (run! elisp-executor)
      (unless (send (root) running-module? this)
        (send* (root)
          (push-running-module! (cons this elisp-executor))
          (run-other-if-left!))))


    (define/public (interrupt-execution!)
      (send (emacs) deferred-call 'pt:interrupt-process))
    
        
    (define/public (mark-as-running!)
      (put-extra-ftf-prop! ':foreground const:execution-foreground-color))
    
    (define/public (mark-as-not-running!)
      (remove-extra-ftf-prop! ':foreground))
    
    (define/public (on-exit-status! code)
      (mark-as-not-running!)
      (match code
        [0 (on-normal-exit-status!)]
        [1 (on-error-exit-status!)]
        [other (error 'unexpected-exit-status "~a" other)])
      (send* (root) (drop-running-module!) (run-other-if-left!)))

    (define/public (on-normal-exit-status!) (void))
    (define/public (on-error-exit-status!) (void))

    (define/public (on-unexpected-status! status)
      (error 'unexpected-process-status "~a" status))



    
    ))




(define-composed-mixins
  [runnable-final-sum (leaf-final-sum runnable)])
