#lang racket/base

(require racket/contract
         racket/format
         racket/string
         racket/match
         racket/function
         racket/list
         racket/pretty
         
         ss/racket/class
         ss/racket/provide
         
         ;"edsl/utils.rkt"
         )


(provide (prefix-out base: (suffixed-as interface mixin class
                                        #:from (all-defined-out))))


(define (node? v)       (is-a? v node<%>))
(define (descendant? v) (is-a? v descendant<%>))
(define (root? v)       (is-a? v root<%>))
(define (intr? v)       (is-a? v intr<%>))
(define (leaf? v)       (is-a? v leaf<%>))
(define (ancestor? v)   (is-a? v ancestor<%>))

(define pprintable-node<%>
  (interface ()
    [represent   (->m natural-number/c string?)]
    [repr-fields (->m (listof symbol?))]
    ))


(define pprintable-node-mixin
  (mixin () (printable<%> pprintable-node<%>)
    (super-new)

    (define/public (represent depth)
      (format "~a~a  ~a"
              (make-string (* 4 depth) #\ )
              (~a this% #:min-width 20)
              (string-join (for/list ([field-symbol (repr-fields)])
                             (format "~a : ~v"
                                     (~a field-symbol)
                                     (dynamic-get-field field-symbol this))))))
    
    
    (define/public (repr-fields) '())
    
    (define/public (custom-write out)
      (write (represent 0) out))
    
    (define/public (custom-display out)
      (display (represent 0) out))
    
    (define/public (custom-print out depth)
      (display (string-append "\n" (represent 0)) out))
    
    
    ))


(define pprintable-ancestor-mixin
  (mixin (pprintable-node<%>) ()
    (super-new)

    (inherit-field children)
    
    (define/override (represent depth)
      (string-append (super represent depth) "\n"
                     (string-join (for/list ([child children])
                                    (send child represent (add1 depth)))
                                  "\n")))

    ))

(define node<%>
  (interface (pprintable-node<%>)    
    ;; [display-tree (->m node?
    ;;                    natural-number/c
    ;;                    any)]
    ;; display-tree
    
    [equality-fields (->m (listof symbol?))]
    
    [find-dfs (->m (-> node? any) any)]
    [depth (->m natural-number/c)]
    [root (->m root?)]
    [last-descendant-or-self (->m node?)]
    [first-leaf (->m (or/c leaf? #f))]
    [last-leaf (->m (or/c leaf? #f))]
    [leafs (->m (listof leaf?))]
    [nodes (->m (listof node?))]
    
    [next-descendant-neighbour (->m (or/c descendant? #f))]))




(define node-mixin  
  (mixin (pprintable-node<%>) (node<%> equal<%>)
    (super-new)

    (abstract depth
              root
              last-descendant-or-self
              first-leaf
              last-leaf
              leafs
              nodes
              next-descendant-neighbour)
                    
    (define/public (equal-to? other recur)
      (match (equality-fields)
        ['() (eq? other this)]
        [(list fields ...) (andmap (λ (fld) (equal? (dynamic-get-field fld this)
                                                    (dynamic-get-field fld other)))
                                   fields)]))
    

    
    
    (define/public (equal-hash-code-of hash-code)
      (error 'not-implemented))

    (define/public (equal-secondary-hash-code-of hash-code)
      (error 'not-implemented))

    (define/public (equality-fields) '())
    
    (define/public (cls) this%)
    
    ;; (define/public (display-tree node-to-highlight depth)
    ;;   (when (eq? node-to-highlight this)
    ;;     (ansi-esc:foreground 'red))
      
    ;;   (displayln (represent depth))
    ;;   (ansi-esc:reset))

    (define/public (find-dfs predicate)
      (if (predicate this) this #f))

    
    
    ))



(define descendant<%>
  (interface (node<%>)
    repr-with-tree 
    
    
    [next-sibling (->m (or/c descendant? #f))]
    [prev-sibling (->m (or/c descendant? #f))]
    [all-ancestors-till (->m ancestor? (listof ancestor?))]
    [all-ancestors-till-exclusive (->m ancestor? (listof ancestor?))]
    [next-neighbour (->m (or/c descendant? #f))]
    [prev-neighbour (->m (or/c descendant? #f))]
    
    [prev-neighbour-for-next-sibling (->m (or/c descendant? #f))]
    [remove! (->m void?)]

    [next-intr-for-prev-neighbour (->m (or/c intr? #f))]
    [prev-intr-for-next-neighbour (->m (or/c intr? #f))]
    [next-intr (->m (or/c intr? #f))]
    [prev-intr (->m (or/c intr? #f))]
    
    [next-leaf-for-prev-neighbour (->m (or/c leaf? #f))]
    [prev-leaf-for-next-neighbour (->m (or/c leaf? #f))]
    [next-leaf (->m (or/c leaf? #f))]
    [prev-leaf (->m (or/c leaf? #f))]
    [circular-next-leaf (->m leaf?)]
    [circular-prev-leaf (->m leaf?)]
    [circular-next-intr (->m intr?)]
    [circular-prev-intr (->m intr?)]
    
    [lift! (->m void?)]
    [lower! (->m void?)]
    [lifting-impossible? (->m boolean?)]
    [lowering-impossible? (->m boolean?)]
    [pre-lift! (->m descendant? descendant? void?)]
    [post-lift! (->m descendant? descendant? void?)]
    [lift-if-possible! (->m void?)]
    [lower-if-possible! (->m void?)]
    ))


(define descendant-mixin  
  (mixin (node<%>) (descendant<%>)
    (field [parent 'uninitialized])
    (super-new)

    (abstract next-neighbour
              prev-neighbour-for-next-sibling
              next-leaf-for-prev-neighbour
              prev-leaf-for-next-neighbour
              next-intr-for-prev-neighbour
              prev-intr-for-next-neighbour)


        
    (define/override (root) (send parent root))
    
    (define/override (depth)
      (add1 (send parent depth)))
    
    
    (define/public (repr-with-tree)
      'fixme)
    
    ;; (send (root) display-tree this))

    (define this? (curry eq? this))

    (define/public (remove!) (send parent remove-child! this))
    
    (define/public (prev-sibling)
      (match (get-field children parent)
        [(list  _ ... prev (? this? _) _ ...) prev]
        [(list (? this? _) _ ...) #f]))
    
    
    (define/public (next-sibling)
      (match (get-field children parent)
        [(list  _ ... (? this? _) next  _ ...) next]
        [(list  _ ... (? this? _)) #f]))
    
    
    (define/public (all-ancestors-till ancestor)
      (if (eq? ancestor parent)
          (list parent)
          (cons parent (send parent all-ancestors-till ancestor))))

    (define/public (all-ancestors-till-exclusive ancestor)
      (if (eq? ancestor parent)
          '()
          (cons parent (send parent all-ancestors-till-exclusive ancestor))))
    
    (define/override (next-descendant-neighbour)
      (or (next-sibling) (send parent next-descendant-neighbour)))


    
    (define/public (prev-neighbour)
      (cond
       [(prev-sibling) => (method prev-neighbour-for-next-sibling)]
       [else (send parent prev-neighbour-for-first-child)]))
        

    (define/public (next-intr)
      (cond
       [(next-neighbour) => (method next-intr-for-prev-neighbour)]
       [else #f]))

    (define/public (prev-intr)
      (cond
       [(prev-neighbour) => (method prev-intr-for-next-neighbour)]
       [else #f]))



    (define/public (circular-next-intr)
      (or (next-intr) (send (root) first-intr)))

    (define/public (circular-prev-intr)
      (or (prev-intr) (send (root) last-intr)))
    

    
    (define/public (next-leaf)
      (cond
       [(next-neighbour) => (method next-leaf-for-prev-neighbour)]
       [else #f]))
    

    (define/public (prev-leaf)
      (cond
       [(prev-neighbour) => (method prev-leaf-for-next-neighbour)]
       [else #f]))


    (define/public (circular-next-leaf)
      (or (next-leaf) (send (root) first-leaf)))

    (define/public (circular-prev-leaf)
      (or (prev-leaf) (send (root) last-leaf)))
    

    (define/public (lift!)
      (match-define (list upper-siblings ...
                          prev
                          (? this? self)
                          lower-siblings ...)
                    (get-field children parent))

      (pre-lift! self prev)
      
      (set-field! children parent (append upper-siblings
                                          (list self prev)
                                          lower-siblings))
      
      (post-lift! self prev))

    
    (define/public (lower!)
      (send (next-sibling) lift!))

    
    (define/public (pre-lift! self prev) (void))
    (define/public (post-lift! self prev) (void))

    (define/public (lifting-impossible?) (eq? (prev-sibling) #f))
    (define/public (lowering-impossible?) (eq? (next-sibling) #f))

    (define/public (lift-if-possible!)
      (unless (lifting-impossible?) (lift!)))
    
    (define/public (lower-if-possible!)
      (unless (lowering-impossible?) (lower!)))
    
    
    
    ))


(define ancestor<%>
  (interface (node<%>)
    [first-intr (->m (or/c intr? #f))]
    [last-intr (->m (or/c intr? #f))]
    
    [init-children! (->m (listof descendant?) void?)]
    [add-children! (->m (listof descendant?) void?)]
    [push-child! (->m descendant? void?)]
    [append-child! (->m descendant? void?)]
    [descendants  (->m (listof descendant?))]
    [next-ancestor-neighbour (->m descendant?)]
    [remove-child! (->m descendant? void?)]
    [has-leafs? (->m boolean?)]
    [has-intrs? (->m boolean?)]
    [prev-neighbour-for-first-child (->m (or/c ancestor? #f))]
    ))


(define ancestor-mixin
  (mixin (node<%>) (ancestor<%>)
    (super-new)
    
    (field [children '()])

    (abstract prev-neighbour-for-first-child)
    
    (define (set-child-parent! child) (set-field! parent child this))
        
    (define set-children-parent! (curry for-each set-child-parent!))
    
    
    (define/public (init-children! v)
      (set! children v)

      (set-children-parent! v))

    (define/override (equality-fields)
      (cons 'children (super equality-fields)))

    (define/public (add-children! v) 
      (set! children (append children v))
      (set-children-parent! v))


    (define/public (push-child! v)
      (set! children (cons v children))
      (set-child-parent! v))

    (define/public (append-child! v)
      (set! children (append children (list v)))
      (set-child-parent! v))


    (define/public (remove-child! child)
      (match children
        [(list prev ... (? (curry eq? child) _) next ...)
         (set! children (append prev next))]))

    
    (define/override (leafs)
      (apply append (map (method leafs) children)))
    
    (define/override (nodes)
      (cons this (descendants)))

    (define/public (descendants)
      (apply append (map (method nodes) children)))
      
    
    (define/public (has-leafs?)
      (ormap (curryr is-a? leaf<%>) (descendants)))

    (define/public (has-intrs?)
      (ormap (curryr is-a? intr<%>) (descendants)))
    
    ;; (define/override (display-tree node-to-highlight depth)
    ;;   (super display-tree node-to-highlight depth)

    ;;   (for ([child children])
    ;;     (send child display-tree node-to-highlight (add1 depth)))
      
    ;;   )


    (define/override (last-descendant-or-self)
      (match children
        ['() this]
        [(list _ ... last) (send last last-descendant-or-self)]))

    (define/override (first-leaf)
      (ormap (method first-leaf) children))

    (define/override (last-leaf)
      (ormap (method last-leaf) (reverse children)))

    
    (define/public (first-intr)
      (findf intr? children))
    
    (define/public (last-intr)
      (cond
       [(findf intr? (reverse children)) => (λ (last-top) (or (send last-top last-intr)
                                                              last-top))]
       [else #f]))
      

    
    (define/override (find-dfs predicate)
      (or (super find-dfs predicate)
          (ormap (curryr (method find-dfs) predicate) children)))


    
    (define/public (next-ancestor-neighbour)
      (and (cons? children) (car children)))

    
    ))



(define root<%>
  (interface (ancestor<%>)
    [make-ht (->m (-> (is-a?/c node<%>) any)
                  hash?)]
    ))




(define root-mixin 
  (mixin (ancestor<%>) (root<%>)
    (super-new)

    (inherit descendants)
    
    (define/override (root) this)
    (define/override (depth) 0)
    
    (define/public (make-ht key-extractor)
      (define descs (descendants))      
      (make-immutable-hash (map cons (map key-extractor descs) descs)))
    
    ;; (define/override (display-tree  . a) ;node-to-highlight)
    ;;   1)
    
    ;; (super display-tree node-to-highlight 0))
    
    ;; (define/override (display-tree node-to-highlight)
    ;;   (super display-tree node-to-highlight 0))

    
    (define/override (prev-neighbour-for-first-child) #f)
    (define/override (next-descendant-neighbour) #f)
    
    ))


(define quasiroot<%>
  (interface (descendant<%> root<%>)
    ))

(define quasiroot-mixin
  (mixin (descendant<%> root<%>) (quasiroot<%>)
    (super-new)
    
    (define/override (prev-neighbour-for-next-sibling) (error 'undefined-on-quasiroot))
    (define/override (next-leaf-for-prev-neighbour) (error 'undefined-on-quasiroot))
    (define/override (prev-leaf-for-next-neighbour) (error 'undefined-on-quasiroot))
    
    (define/override (next-intr-for-prev-neighbour) (error 'undefined-on-quasiroot))
    (define/override (prev-intr-for-next-neighbour) (error 'undefined-on-quasiroot))
    
    
    
    ))


(define intr<%>
  (interface (ancestor<%> descendant<%>)
    ))

(define intr-mixin
  (mixin (ancestor<%> descendant<%>) (intr<%>)
    (super-new)
    
    (inherit next-ancestor-neighbour
             next-descendant-neighbour
             last-descendant-or-self
             next-leaf
             prev-leaf)

    (define/override (next-neighbour)
      (or (next-ancestor-neighbour) (next-descendant-neighbour)))
    

    (define/override (prev-neighbour-for-next-sibling) (last-descendant-or-self))
    (define/override (prev-neighbour-for-first-child)  this)

    (define/override (next-intr-for-prev-neighbour) this)
    (define/override (prev-intr-for-next-neighbour) this)
    
    (define/override (next-leaf-for-prev-neighbour) (next-leaf))
    (define/override (prev-leaf-for-next-neighbour) (prev-leaf))
    

    ))



(define leaf<%>
  (interface (descendant<%>)
    ))

(define leaf-mixin
  (mixin (descendant<%>) (leaf<%>)
    (super-new)
    
    (inherit next-descendant-neighbour
             next-intr
             prev-intr)

    (define/override (next-neighbour)
      (next-descendant-neighbour))

    (define/override (last-descendant-or-self) this)

    (define/override (prev-neighbour-for-next-sibling) this)
    (define/override (first-leaf) this)
    (define/override (last-leaf) this)
    (define/override (leafs) (list this))
    (define/override (nodes) (list this))

    (define/override (next-intr-for-prev-neighbour) (next-intr))
    (define/override (prev-intr-for-next-neighbour) (prev-intr))
    
    (define/override (next-leaf-for-prev-neighbour) this)
    (define/override (prev-leaf-for-next-neighbour) this)

    ))


(define-composed-mixins
  [node-sum       (pprintable-node node)]
  [ancestor-sum   (node-sum ancestor pprintable-ancestor)]
  [descendant-sum (node-sum descendant)]
  
  [leaf-sum       (descendant-sum leaf)]
  [intr-sum       (ancestor-sum descendant intr)]
  [root-sum       (ancestor-sum root)]
  [quasiroot-sum  (ancestor-sum descendant root quasiroot)])



(define leaf% (leaf-sum-mixin object%))
(define intr% (intr-sum-mixin object%))
(define root% (root-sum-mixin object%))

