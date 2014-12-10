#lang racket/base

(require racket/contract
         racket/list
         racket/function
         racket/pretty
         
         ss/racket/class
         ss/racket/provide
         
         "base.rkt")


(provide (prefix-out serialization: (suffixed-as interface mixin class
                                                 #:from (all-defined-out)))
         read-node)

;; (define (id-number-init-predicate obj)

;;   )

(define (node-with-initialized-id-number? node)
  (natural-number/c (get-field id-number node)))

(define (read-node path ns)
  (eval (call-with-input-file path read) ns))


(define (node? v)       (is-a? v node<%>))
(define (descendant? v) (is-a? v descendant<%>))

(define node<%>
  (interface ()
    [enumerate-tree! (->m void?)]
    [node-identifier (-> node-with-initialized-id-number? symbol?)]
    [node-constructor (->m list?)]
    [serialize (->m list?)]
    [cache-node! (->m path-string? void?)]
    [tree-constructor-exprs (->m (listof list?))]
    [field-setter-exprs (->m (listof list?))]
    [serialization-nodes (->m (listof node?))]
    ))


(define node-mixin
  (mixin (base:node<%>) (node<%> inspectable<%>)
    (super-new)

    (field [id-number 'uninitialized])

    (abstract serialization-nodes)

    (define/public (get-class-name) 'unspecified)
    
    
    ;; (define/public (set-id-number v) (set! id-number v))
    
    ;; (define/public (tree-constructor-exprs) `(new ,(get-class-name)))
    ;; (define/public (tree-identifiers) (node-identifier))


    (define/public (node-constructor) `(new ,(get-class-name)))
    
    (define/public (tree-constructor-exprs)
      (map (method node-constructor) (serialization-nodes)))
    
    (define/public (node-identifier) (string->symbol (format "n-~a" id-number)))

    (define/public (tree-identifiers)
      (map (method node-identifier) (serialization-nodes)))

    (define/public (enumerate-tree!)
      (define tree (serialization-nodes))
      (for-each (λ (node num) (set-field! id-number node num))
                tree
                (range 0 (length tree))))
    
    (define/public (field-setter-exprs) '())

    (define/public (cache-node! path)
      (call-with-output-file path (curry pretty-write (serialize)) #:exists 'replace))



    
    
    (define/public (serialize)
      (enumerate-tree!)
      
      `(let (,@(map list (tree-identifiers) (tree-constructor-exprs)))
         ,@(apply append (map (method field-setter-exprs) (serialization-nodes)))
         ;; n-0 — first and this node
         n-0))

    
    
    ))

(define ancestor<%>
  (interface (node<%>)
    [serialization-descendants (->m (listof descendant?))]))

(define ancestor-mixin
  (mixin (base:ancestor<%> node<%>) (ancestor<%>)
    (super-new)

    (inherit-field children)
    (inherit node-identifier)
    
    (define/override (serialization-nodes)
      (cons this (serialization-descendants)))

    (define/public (serialization-descendants)
      (apply append (map (method serialization-nodes) children)))

    
    (define/override (field-setter-exprs)
      (cons `(send ,(node-identifier)
                   init-children!
                   (list ,@(map (method node-identifier) children)))
            (super field-setter-exprs)))
    
    ))


(define descendant<%>
  (interface (node<%> base:descendant<%>)
    ))

(define descendant-mixin
  (mixin (base:descendant<%> node<%>) (descendant<%>)
    (super-new)))




(define leaf<%>
  (interface (descendant<%> base:leaf<%>)
    ))


(define leaf-mixin
  (mixin (base:leaf<%> descendant<%>) (leaf<%>)
    (super-new)

    (define/override (serialization-nodes) (list this))
    
    ))


(define intr<%>
  (interface (descendant<%> ancestor<%>)
    ))

(define intr-mixin
  (mixin (base:intr<%> descendant<%> ancestor<%>) (intr<%>)
    (super-new)))


(define root<%>
  (interface (ancestor<%>)
    ))

(define root-mixin
  (mixin (base:root<%> ancestor<%>) (root<%>)
    (super-new)))


(define-composed-mixins
  [ancestor-sum   (node ancestor)]
  [descendant-sum (node descendant)]

  [leaf-sum       (descendant-sum leaf)]
  [intr-sum       (ancestor-sum descendant intr)]
  [root-sum       (ancestor-sum root)])
