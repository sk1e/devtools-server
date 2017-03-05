#lang racket/base

(require racket/list
         racket/contract
         racket/match
         racket/function
         
         ss/racket/class
         ss/racket/provide

         "base.rkt"
         "serialization.rkt")

(provide (prefix-out segment: (suffixed-as interface mixin #:from (all-defined-out))))

(define-logger segment)

(define node<%>
  (interface (base:node<%> serialization:node<%>) 
    [shift-lower-nodes! (->m integer? void?)]
    ))

(define node-mixin
  (mixin (base:node<%> serialization:node<%>) (node<%>)
    (super-new)

    (abstract shift-lower-nodes!)
    ))

(define descendant<%>
  (interface (base:descendant<%> serialization:descendant<%> node<%>)
    [init-point! (->m natural-number/c void?)]
    
    [resultant-point (->m natural-number/c)]
    [resultant-offset (->m integer?)]
    [subtree-end-point   (->m natural-number/c)]
    
    [shift-point! (->m integer? void?)]
    
    [infer-point! (->m void?)]
    ))



(define descendant-mixin
  (mixin (base:descendant<%> serialization:descendant<%> node<%>) (descendant<%>)
    (super-new)
    
    (field [point 'uninitialized])

    (abstract subtree-end-point)
    
    (inherit-field parent)
    (inherit all-ancestors-till-exclusive
             root
             prev-sibling
             node-identifier)


    (define/override (field-setter-exprs)
      (cons `(send ,(node-identifier) init-point! ,point)
            (super field-setter-exprs)))

    (define/public (init-point! v) (set! point v))
    

    (define/public (resultant-offset)
      (apply + (map (field-getter point) (all-ancestors-till-exclusive (root)))))
    
    (define/public (resultant-point)
      (+ point (resultant-offset))) 

    
    (define/public (shift-point! v)
      (set! point (+ point v)))
    
    
    (define/override (shift-lower-nodes! v)
      (match (get-field children parent)
        [(list _ ... (? (λ (node) (eq? node this)) _) lower-siblings ...)
         
         (for ([sib lower-siblings])
           (send sib shift-point! v))

         (send parent shift-lower-nodes! v)]))


    
    (define/public (infer-point!)
      (set! point (cond
                   [(prev-sibling) => (method subtree-end-point)]
                   [else (send parent point-for-first-child)])))
    
    
    
    ))

(define ancestor<%>
  (interface (node<%> base:ancestor<%> serialization:ancestor<%>)
    [point-for-first-child (->m natural-number/c)]
    
    ))


(define ancestor-mixin
  (mixin (node<%> base:ancestor<%> serialization:ancestor<%>) (ancestor<%>)    
    (super-new)
    
    (abstract point-for-first-child)

    ))


(define intr<%>
  (interface (descendant<%> ancestor<%>)
    [shift-children! (->m integer? void?)]
    ))

(define intr-mixin
  (mixin (base:intr<%> descendant<%> ancestor<%>) (intr<%>)
    (super-new)

    (inherit-field children)

    (define/public (shift-children! v)
      (for-each (curryr (method shift-point!) v) children))

    ))



(define root<%>
  (interface (base:root<%> ancestor<%> serialization:root<%>) 
    ;[infer-points! (->m void?)]
    ))

(define root-mixin
  (mixin (base:root<%> ancestor<%> serialization:root<%>) (root<%>)
    (super-new)

    (inherit descendants)
    
    (define/override (shift-lower-nodes! v) (void))

    (define/override (point-for-first-child) 1)

    ;; (define/public (infer-points!)
    ;;   (for-each (method infer-point!) (descendants)))

    
    ))


(define-composed-mixins

  [leaf-final-sum (base:leaf-sum serialization:leaf-sum node descendant)]
  [intr-final-sum (base:intr-sum serialization:intr-sum node descendant ancestor intr)]
  [root-final-sum (base:root-sum serialization:root-sum node ancestor root)]
  [quasiroot-final-sum (base:quasiroot-sum serialization:root-sum node ancestor root)])



;; (define-composed-mixins

;;   [leaf-final-sum (base:leaf-sum node descendant)]
;;   [intr-final-sum (base:intr-sum node descendant ancestor)]
;;   [root-final-sum (base:root-sum node ancestor root)]
;;   [quasiroot-final-sum (base:quasiroot-sum node ancestor root)])

