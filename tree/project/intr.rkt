#lang racket/base

(require racket/unit
         racket/contract
         
         ss/racket/class
         
         "descendant-sig.rkt"
         "ancestor-sig.rkt"
         
         "descendant-unit.rkt"
         "ancestor-unit.rkt"         

         "module.rkt"
         "../file.rkt"
         "../ebuffer.rkt"
         )


(provide intr<%>
         child-directory-mixin
         directory%)

(define-compound-unit/infer linked@
  (import )
  (export descendant^ ancestor^)
  (link descendant@ ancestor@))

(define-values/invoke-unit/infer linked@)

(define intr<%>
  (interface (file:intr<%> ebuffer:intr<%> descendant<%> ancestor<%>)
    ))


(define intr-mixin
  (mixin (file:intr<%> descendant<%> ancestor<%>) (intr<%>)
    (super-new)

    (inherit-field children)

    (define/override (pre-rename! new-name)
      (for-each (λ (node)
                   (send node
                         pre-rename!
                         (path->string (build-path new-name
                                                   (get-field name node)))))
                children))
    
    (define/override (post-select!) (void))
    (define/override (initialize-project-node!) (void))
    ;; (define/override (face) 'pt:intr-face)
    (define/override (entered-directory) this)
    
    ))


(define child-directory-mixin
  (mixin (file:ancestor<%>) ()
    (super-new)

    (define/override (child-directory%) directory%)))


(define-inspected-class directory% (class-from-mixins ebuffer:intr-final-sum
                                                      file:intr-sum
                                                      descendant-sum
                                                      ancestor
                                                      intr
                                                      child-directory
                                                      child-file))




;; (define-composed-mixins
;;   [descendant-sum (node descendant)]
 
;;   [root-sum       (node ancestor root)]
;;   [intr-sum       (descendant-sum ancestor intr)]
;;   [leaf-sum       (descendant-sum leaf)])


;; (define-composed-mixins  
;;   [leaf-final-sum (ebuffer:leaf-final-sum      file:leaf-sum leaf-sum)]
;;   [module-leaf-final-sum (leaf-final-sum       module-leaf)]
;;   [intr-final-sum (ebuffer:intr-final-sum      file:intr-sum intr-sum)]
;;   [root-final-sum (ebuffer:quasiroot-final-sum file:intr-sum root-sum)])


;; (define-inspected-class file%        (class-from-mixins leaf-final-sum))
;; (define-inspected-class module%      (class-from-mixins leaf-final-sum runnable-leaf module-leaf))
;; (define-inspected-class module-test% (class-from-mixins leaf-final-sum runnable-leaf test-leaf))
;; (define-inspected-class directory%   (class-from-mixins intr-final-sum))
;; (define-inspected-class root%        (class-from-mixins root-final-sum))
