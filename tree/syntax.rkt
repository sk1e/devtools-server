#lang racket/base

(require racket/contract
         racket/list
         racket/function
         
         ss/racket/class
         ss/racket/provide
         
         "base.rkt"
         "segment.rkt")



(provide (prefix-out syntax: (suffixed-as interface mixin #:from (all-defined-out))))


(define descendant<%>
  (interface (base:descendant<%> segment:descendant<%>)
    [syntax-node->string (->m string?)]
    [nonseparator? (->m any)]
    [form? (->m string? any)]
    ))


(define descendant-mixin
  (mixin (base:descendant<%> segment:descendant<%>) (descendant<%>)
    (inspect #f)
    (super-new)
    
    (inherit-field point)
        
    (inherit-field parent)
    (inherit prev-sibling all-ancestors-till)

    (abstract nonseparator?
              form?
              syntax-node->string)
    


    ;; for testing uses
    (define/public (point-provider-info)
      (define prev (prev-sibling))
      (apply (curry format "~a point provider: ~a ~a" this%)
             (if prev
                 (list (send prev cls) "point for next sibling")
                 (list (send parent cls) "point for first child"))))
        
    ))


(define ancestor<%>
  (interface (base:ancestor<%>)
    ))

(define ancestor-mixin
  (mixin (base:ancestor<%>) (ancestor<%> equal<%>)
    (inspect #f)
    
    (inherit-field children)
    
    (super-new)
    
    (define/public (equal-to? other recur)
      (equal? children (get-field children other)))
    
    (define/public (equal-hash-code-of hash-code)
      (hash-code children))
    
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code children))

    
    ;(abstract point-for-first-child)
    
    ))


(define root<%>
  (interface (ancestor<%>)
    ;; [display-module (->m (is-a?/c descendant<%>) void?)]
    [module->string (->m string?)]))




(define root-mixin  
  (mixin (ancestor<%>) (root<%>)
    (inspect #f)
    
    (inherit descendants
             ;display-tree
             )
    (inherit-field children)
    (super-new)
    
    ;; (define/public (display-module node-to-highlight)
    ;;   (display-tree node-to-highlight 0))
    
    
    (define/public (module-node) this)
        
    (define/public (module->string)
      (apply string-append (map (method syntax-node->string)
                                children)))
    
    
    ))


(define intr<%>
  (interface (descendant<%> ancestor<%>)
    [suffix-length (->m natural-number/c)]
    [prefix-length (->m natural-number/c)]
    ))

(define intr-mixin
  (mixin (ancestor<%> descendant<%> segment:descendant<%> segment:ancestor<%>) (intr<%>)
    (inspect #f)
    (super-new)
    
    (field [left-prefix ""]
           [left-suffix ""]
           [right-prefix ""]
           [right-suffix ""])

    (inherit last-descendant-or-self)
    (inherit-field children point)

    (define/public (init-left-prefix v) (set! left-prefix v))
    (define/public (init-right-prefix v) (set! right-prefix v))
    
    (define/public (init-left-suffix v) (set! left-suffix v))
    (define/public (init-right-suffix v) (set! right-suffix v))
    
    
    (define/public (central-prefix) "")
    (define/public (central-suffix) "")
    
      
    (define/public (prefix-length)
      (apply + (map string-length (list left-prefix (central-prefix) right-prefix))))

    (define/public (suffix-length)
      (apply + (map string-length (list left-suffix (central-suffix) right-suffix))))
    

    (define/override (syntax-node->string)
      (string-append left-prefix
                     (central-prefix)
                     right-prefix
                     (apply string-append (map (method syntax-node->string)
                                               children))
                     left-suffix
                     (central-suffix)
                     right-suffix))
    
    

    (define/override (subtree-end-point)
      (cond
       [(null? children)
        (+ (prefix-length) (suffix-length) point)]
       
       [else
        (define last-descendant (last-descendant-or-self))
        (apply + 
               (send last-descendant subtree-end-point)
               (map (λ (a) (+ (get-field point a)
                              (send a suffix-length)))
                    (send last-descendant all-ancestors-till this)))]))
    
    
    (define/override (point-for-first-child) (prefix-length))
    
    ))

(define leaf<%>
  (interface (descendant<%>)
    [prefix (->m string?)]
    [init-name (->m string? void?)]))

(define leaf-mixin
  (mixin (descendant<%>) (leaf<%> equal<%>)
    
    (inspect #f)
    (super-new)
    
    (inherit-field point)
    
    (field [name 'uninitialized])

  (define/public (init-name v)
    (set! name v))
  
  (define/override (repr-fields)
    (cons 'name (super repr-fields)))
  
  (define/public (equal-to? other recur)      
    (equal? name (get-field name other)))

  (define/public (equal-hash-code-of hash-code)
    (hash-code name))

  (define/public (equal-secondary-hash-code-of hash-code)
    (hash-code name))
  
  
  (define/public (prefix) "")
  

  (define/override (syntax-node->string)
    (string-append (prefix) name))

  (define/override (subtree-end-point)
    (+ point (string-length (syntax-node->string))))
  
  
  ))


;; (define-composed-mixins
;;  [leaf-sum (descendant leaf)])
;;  [intr-sum (descendant ancestor intr)]
;;  [root-sum (ancestor root)]

(define-composed-mixins
  [leaf-sum (segment:leaf-final-sum descendant leaf)]
  [intr-sum (segment:intr-final-sum descendant ancestor intr)]
  [root-sum (segment:root-final-sum ancestor root)])



(define separator-mixin
  (mixin (descendant<%>) ()
    (inspect #f)
    (super-new)
    
    (define/override (nonseparator?) #f)))

(define nonseparator-mixin
  (mixin (descendant<%>) ()
    (inspect #f)
    (super-new)
    
    (define/override (nonseparator?) #t)))

(define nonform-mixin
  (mixin (descendant<%>) ()
    (inspect #f)
    (super-new)

    (define/override (form? name) #f)))

