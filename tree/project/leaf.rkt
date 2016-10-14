#lang racket/base

(require racket/unit
         racket/list
         racket/contract

         ss/racket/class
         
         "descendant-sig.rkt"
         
         "descendant-unit.rkt"
         "ancestor-unit.rkt"         
         
         "../file.rkt"
         "../ebuffer.rkt"
         "../../constants.rkt"
         "../../backend/emacs.rkt")

(provide leaf<%>
         leaf-final-sum-mixin
         file%)



(define-compound-unit/infer linked@
  (import )
  (export descendant^)
  (link descendant@ ancestor@))

(define-values/invoke-unit/infer linked@)


(define leaf<%>
  (interface (file:leaf<%> descendant<%>)
    [buffer-name (->m string?)]
    [switch-to-buffer! (->m void?)]
    [modified-indicator (->m ebuffer:indicator?)]
    [test-indicator (->m ebuffer:indicator?)]
    ))




(define leaf-mixin
  (mixin (file:leaf<%> descendant<%>) (leaf<%>)
    (super-new)

    (inherit absolute-path
             project-path
             root)
    
    (inherit-field parent
                   indicators)


    ;; todo make private
    (field [buffer 'uninitialized])

    
    ;; (define/override (face) 'pt:leaf-face)
    
    (define/override (indicator-list)
      (list (make ebuffer:modified-indicator%
              [position 0]
              [color const:indicator-modified-warning-color]
              [node this])
            (make ebuffer:test-indicator%
              [position 1]
              [color const:indicator-test-base-color]
              [node this])))
    

    (define/public (modified-indicator) (first indicators))
    (define/public (test-indicator) (second indicators))
    
    (define/override (post-select!)
      (switch-to-buffer!))

    (define/override (entered-directory) parent)


    (define/public (switch-to-buffer!)
      (send (emacs) deferred-call 'switch-to-buffer (get-field name buffer)))

    
    (define/public (buffer-name)
      (path->string (project-path)))

    (define (buffer-name-font-parts path)
      (define-values (base name _) (split-path path))
      (list (list (path->string base)
                  const:buffer-base-path-face)
            (list (path->string name)
                  const:buffer-name-path-face)))
    
    (define/override (initialize-project-node!)
      (define name (buffer-name))
      (set! buffer (new (send (emacs) buffer%) [name name]))
      (send (emacs) deferred-call 'pt:init-file-buffer
                     (path->string (absolute-path))
                     name
                     (buffer-name-font-parts name)))

    
    (define/override (pre-rename! new-name)
      (set! buffer (new (send (emacs) buffer%) [name new-name]))
      (send (emacs) deferred-call 'pt:rename-file-buffer
                     (buffer-name)
                     new-name
                     (buffer-name-font-parts new-name)))
    
    
    
    ))



(define-composed-mixins
  [leaf-final-sum (ebuffer:leaf-final-sum file:leaf-sum descendant-sum leaf)])


(define-inspected-class file% (class-from-mixins leaf-final-sum))

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
