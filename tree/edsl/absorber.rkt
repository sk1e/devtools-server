#lang racket/base

(require ss/racket/provide
         ;ss/racket/class

         (for-syntax racket/base
                     racket/function
                     racket/contract

                     ss/racket/class
                     ss/racket/syntax
                     
                     syntax/stx
                     syntax/parse

                     "../base.rkt"))


;; ;(provide (prefix-out absorber: (suffixed-as interface mixin #:from (all-defined-out))))
(provide (for-syntax (all-defined-out)))
;; (struct-out call-annotation)
;; (struct-out value-annotation)
;; (struct-out annotation))

;; (struct annotation (node) #:transparent)
;; (struct call-annotation annotation (methid) #:transparent)
;; (struct value-annotation annotation (value) #:transparent)

(begin-for-syntax 

(define (root? %) (is-a? % root<%>))
(define (leaf? %) (is-a? % leaf<%>))
(define (intr? %) (is-a? % intr<%>))

(define node<%>
  (interface (base:node<%>)
    [absorb (->m syntax? (or/c syntax? void?))]
    [absorb-annotation (->m syntax? syntax?)]
    
    [leaf% (->m leaf?)]
    [intr% (->m intr?)]
    [root% (->m root?)]

    [class-id (->m identifier?)]
    [produce-constructor-expr (->m syntax?)]
    [rest-constructor-bindings (->m syntax?)]
    ))

(define node-mixin
  (mixin (base:node<%>) (node<%>)
    (super-new)

    (field [sym 'uninitialized])

    (abstract class-id
              ;; leaf%
              ;; intr%
              ;; root%
              )

    (define/public (leaf%) AU-leaf%)
    (define/public (intr%) AU-intr%)
    (define/public (root%) AU-root%)
    
    
    (define/override (equality-fields)
      (cons 'sym (super equality-fields)))

    (define/override (repr-fields)
      (cons 'sym (super repr-fields)))

    (define/public (init-sym v) (set! sym v))
    
    
    (define/public (absorb stx)
      (syntax-parse stx
        [(x:id xs ...)
         (init-sym (syntax-e #'x))
         (absorb-annotation #'(xs ...))
         ]))

    (define/public (absorb-annotation xs) xs)

    (define/public (produce-constructor-expr)
      (with-syntax ([class (class-id)]
                    [name-value sym]
                    [(rest-bindings ...) (rest-constructor-bindings)])
        
        #'(make class
            [name 'name-value]
            rest-bindings ...)))

    (define/public (rest-constructor-bindings) #'())
    
    ))

(define ancestor<%>
  (interface (node<%> base:ancestor<%>)
    [absorb-children (->m syntax? (listof (is-a?/c base:descendant<%>)))]
    ))

(define ancestor-mixin
  (mixin (node<%> base:ancestor<%>) (ancestor<%>)
    (super-new)

    (inherit intr%
             leaf%

             init-children!)
    
    (inherit-field sym children)
    
    (define/override (absorb stx)
      (init-children! (absorb-children (super absorb stx))))
    
    
    (define/public (absorb-children xs)
      (syntax-parse xs
        [() '()]
        
        [((x ...) xs ...)
         (define inter (new (intr%)))
         (send inter absorb #'(x ...))
         (cons inter (absorb-children #'(xs ...)))]
        
        [(x ...)
         (define leaf (new (leaf%)))

         (cons leaf (absorb-children (send leaf absorb #'(x ...))))]))


    (define/override (rest-constructor-bindings)
      (with-syntax ([(child-constructor ...) (map (method produce-constructor-expr) children)])
        #'([children (list child-constructor ...)])))
    
    
    ))


(define leaf<%>
  (interface (node<%>)
    ))

(define leaf-mixin
  (mixin (node<%>) (leaf<%>)
    (super-new)
    
    (define/override (class-id) #'leaf%)
    
    ))


(define intr<%>
  (interface (ancestor<%>)
    ))


(define intr-mixin
  (mixin (ancestor<%>) (intr<%>) 
    (super-new)

    (define/override (class-id) #'intr%)
    ))




(define root<%>
  (interface (ancestor<%>)
    [root-binding [->m syntax?]]
    [produce-descendant-bindings (->m (listof syntax?))]
    [produce-annotation-expr (->m syntax?)]
    ))

(define root-mixin
  (mixin (ancestor<%>) (root<%>)
    (super-new)
    
    (field [node-ht 'uninitialized])

    (inherit descendants produce-constructor-expr)
    
    
    ;(inherit descendants produce-constructor-expr)
    (inherit-field sym)


    (define/override (class-id) #'root%)
    
    (define/public (root-binding)
      (with-syntax ([constructor (produce-constructor-expr)])
        #'(sym constructor)))


    (define/public (produce-descendant-bindings)
      (map (λ (node)
              (with-syntax ([name (get-field sym node)])
                #'(name (hash-ref descendant-ht 'name))))
           (descendants)))


    (define/public (produce-annotation-expr) #''())
    
    ;; (define/override (absorb stx)
    ;;   (super absorb stx)
    ;;   (define descs (descendants))
    ;;   (set! node-ht (make-immutable-hasheq (map cons
    ;;                                             (map (field-getter sym) descs)
    ;;                                             descs))))

    

    
    ))



(define (initialized? v) (not (eq? v 'uninitialized)))

;; (define-struct/contract annotation ([node (is-a?/c base:node<%>)]) #:transparent)
;; (define-struct/contract (call-annotation annotation) ([method symbol?]) #:transparent)
;; (define-struct/contract (value-annotation annotation) ([value any/c]) #:transparent)






(define annotated-node<%>
  (interface (node<%>)
    [get-annotation-value (->m any)]
    ))

(define annotated-node-mixin
  (mixin (node<%>) (annotated-node<%>)
    (super-new)
    
    (field [annotation-value 'uninitialized])
    
    (define/public (get-annotation-value) annotation-value)
    
    ))


(define annotated-root<%>
  (interface (root<%> annotated-node<%>)
    [annotated-nodes (->m (listof (is-a?/c base:node<%>)))]
    [struct-constructor (->m identifier?)]
    
    ))

(define annotated-root-mixin
  (mixin (root<%> annotated-node<%>) (annotated-root<%>)
    (super-new)

    (inherit descendants)
        
    (define/public (annotated-nodes)
      (filter (compose initialized? (field-getter annotation-value))
              (cons this (descendants))))



    (define/override (produce-annotation-expr)
      (define nodes (annotated-nodes))
      (with-syntax ([constructor (struct-constructor)]
                    [(node-arg ...) (map (field-getter sym) nodes)]
                    [(value-arg ...) (map (method get-annotation-value) nodes)])
        #'(list (constructor node-arg value-arg) ...))
      )

    (define/public (struct-constructor) #'no-constructor)

    

    ))

(define value-annotated-node-mixin
  (mixin (annotated-node<%>) ()
    (super-new)

    (inherit root)
    (inherit-field annotation-value)

    
    
    (define/override (absorb-annotation xs)
      (syntax-parse xs
        [((~datum ->) v:expr nxs ...)
         (set! annotation-value (syntax-e #'v))
         #'(nxs ...)]
        [_ xs]))


    (define/override (get-annotation-value)
      (with-syntax ([value annotation-value])
        (cond
         [(not (identifier? #'value)) #'value]
         [(findf (λ (n) (eq? (get-field sym n)
                            annotation-value))
                 (send (root) descendants)) #'value]
         [else #''value])))

    (define/override (leaf%) AVA-leaf%)
    (define/override (intr%) AVA-intr%)
    (define/override (root%) AVA-root%)


    ))



(define call-annotated-node-mixin
  (mixin (annotated-node<%>) ()
    (super-new)
    
    (inherit-field annotation-value)
    
    
    
    (define/override (absorb-annotation xs)
      (syntax-parse xs
        [((~datum <-) v:id nxs ...)
         (set! annotation-value (syntax-e #'v))
         #'(nxs ...)]
        [_ xs]))

    (define/override (leaf%) ACA-leaf%)
    (define/override (intr%) ACA-intr%)
    (define/override (root%) ACA-root%)


    ))





(define value-annotated-root-mixin
  (mixin (annotated-root<%>) ()
    (super-new)

    (inherit find-dfs)
    (inherit-field node-ht)

    (define/override (struct-constructor) #'value-annotation)


    ))



(define call-annotated-root-mixin
  (mixin (annotated-root<%>) ()
    (super-new)
    
    (define/override (struct-constructor) #'call-annotation)
    
    ;; (define/override (node+value->annotation) call-annotation)

    ))


(define-composed-mixins
  [ancestor-sum (node ancestor)]
  
  [leaf-sum (base:leaf-sum node leaf)]
  [intr-sum (base:intr-sum ancestor-sum intr)]
  [root-sum (base:root-sum ancestor-sum root)]

  )


;; (define (final-refiner leaf intr root)
  
;;   (define (refined node) ((final-refiner leaf intr root) node))
  
;;   (mixin (node<%>) ()
;;     (inspect #f)
;;     (super-new)


;;     (define/override (leaf%) (refined leaf))
;;     (define/override (intr%) (refined intr))
;;     (define/override (root%) (refined root))
    
;;     ))



;; (define (refined-classes leaf intr root)
;;   (define refiner (final-refiner leaf intr root))
;;   ;(displayln (send (new leaf) class-id))
;;   (values (refiner leaf)
;;           (refiner intr)
;;           (refiner root)))


;; (define-values (AU-leaf% AU-intr% AU-root%)
;;   (refined-classes (leaf-sum-mixin object%)
;;                    (intr-sum-mixin object%)
;;                    (root-sum-mixin object%)))

;; (define-values (ACA-leaf% ACA-intr% ACA-root%)
;;   (refined-classes (class-from-mixins leaf-sum annotated-node call-annotated-node)
;;                    (class-from-mixins intr-sum annotated-node call-annotated-node)
;;                    (class-from-mixins root-sum
;;                                       annotated-node
;;                                       call-annotated-node
;;                                       annotated-root
;;                                       call-annotated-root)))

;; (define-values (AVA-leaf% AVA-intr% AVA-root%)
;;   (refined-classes (class-from-mixins leaf-sum annotated-node value-annotated-node)
;;                    (class-from-mixins intr-sum annotated-node value-annotated-node)
;;                    (class-from-mixins root-sum
;;                                       annotated-node
;;                                       value-annotated-node
;;                                       annotated-root
;;                                       value-annotated-root)))



(define AU-leaf% (class-from-mixins leaf-sum))
(define AU-intr% (class-from-mixins intr-sum))
(define AU-root% (class-from-mixins root-sum))

(define ACA-leaf% (class-from-mixins leaf-sum annotated-node call-annotated-node))
(define ACA-intr% (class-from-mixins intr-sum annotated-node call-annotated-node))
(define ACA-root% (class-from-mixins root-sum
                                     annotated-node
                                     call-annotated-node
                                     annotated-root
                                     call-annotated-root))

(define AVA-leaf% (class-from-mixins leaf-sum annotated-node value-annotated-node))
(define AVA-intr% (class-from-mixins intr-sum annotated-node value-annotated-node))
(define AVA-root% (class-from-mixins root-sum
                                     annotated-node
                                     value-annotated-node
                                     annotated-root
                                     value-annotated-root))




;; (define-syntax (with-tree stx)
  
;;   (syntax-parse stx
;;     [(_ #:annotation annotation:id
;;         ;#:node-prefix prefix:id
;;         #:nodes (root:id desc ...)
;;         body ...+)

     
;;      ;(define root-absorber (new (eval-syntax (format-id stx "~a-root%" (syntax-e #'annotation)))))
;;      ;(define root-absorber (new AU-root%))

;;      (send root-absorber absorb #'(root desc ...))
     
;;      (with-syntax ([constructor-expr          (send root-absorber produce-constructor-expr)]
;;                    [annotation-expr           (send root-absorber produce-annotation-expr)]
;;                    [(descendant-bindings ...) (send root-absorber produce-descendant-bindings)])
;;        #'(let (
;;                [root constructor-expr]
;;                [annotations annotation-expr]
;;                [descendant-ht (send root make-ht)]
;;                descendant-bindings ...)
;;            body ...))]))

;; (with-tree #:annotation ACA
;;            #:nodes (root-0 leaf-1
;;                            leaf-2 <- change-state
;;                            (inter-3 leaf-31
;;                                     leaf-32
;;                                     leaf-33))
;;            'ok
;;            )



;; (define root (new AVA-root%))

;; (send root absorb #'(root-0 -> ok
;;                             leaf-1 -> 1
;;                             leaf-2 -> leaf-1
;;                             (inter-33 leaf-31 -> qwe
;;                                       leaf-32)
;;                             leaf-4))

;; ;; (pretty-display (syntax->datum  (send root produce-descendant-bindings)))
;; ;; (pretty-display (map syntax->datum (send root produce-descendant-bindings)))
;; (pretty-display (syntax->datum  (send root produce-constructor-expr)))

;; ;; (send root absorb #'(root-0 leaf-1
;; ;;                             leaf-2
;; ;;                             (intr-3 leaf-31
;; ;;                                     leaf-32)
;; ;;                             leaf-4))


)
