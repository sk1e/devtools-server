#lang racket/base

(require ss/racket/provide
         ss/racket/class
         
         "utils.rkt"
         "../base.rkt"

         racket/function
         racket/stxparam
         rackunit
         
         (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/contract
                     racket/pretty
                     racket/stxparam
                     racket/match

                     
                     ss/racket/class
                     ss/racket/syntax
                     
                     syntax/parse
                     syntax/stx
                     
                     
                     "utils.rkt"
                     "../base.rkt"
                     ))

(provide (all-defined-out))



(define-syntax-parameter root-id #f)
(define-syntax-parameter intr-id #f)
(define-syntax-parameter leaf-id #f)

(begin-for-syntax 
 (define (initialized? v) (not (eq? v 'uninitialized)))

 (define/contract (introduce sym)
   (-> symbol? syntax?)
   (syntax-local-introduce (datum->syntax #f sym)))

 (define node<%>
   (interface (base:node<%>)
     [absorb (->m syntax? (or/c syntax? void?))]
     [absorb-annotation (->m syntax? syntax?)]
     [produced-class-id (->m identifier?)]
     [produce-constructor-expr (->m syntax?)]
     [rest-constructor-bindings (->m syntax?)]
     ))

 (define node-mixin
   (mixin (base:node<%>) (node<%>)
     (super-new)

     (field [sym 'uninitialized])

     (abstract produced-class-id)

     (define/public (get-class-id)
       (define class (produced-class-id))
       (cond
        [(syntax-parameter-value class) class]
        [else (raise-syntax-error 'bad-tree-syntax "output node classes not parameterized with `with-nodes'" class)]))
     
     
     (define/override (equality-fields)
       (cons 'sym (super equality-fields)))

     (define/override (repr-fields)
       (cons 'sym (super repr-fields)))

     (define/public (init-sym! v) (set! sym v))
     
     
     (define/public (absorb stx)
       (syntax-parse stx
         [(x:id xs ...)
          (init-sym! (syntax-e #'x))
          (absorb-annotation #'(xs ...))
          ]))

     (define/public (absorb-annotation xs) xs)

     (define/public (produce-constructor-expr)
       (with-syntax ([class (get-class-id)]
                     [node-sym sym]
                     [(rest-bindings ...) (rest-constructor-bindings)])
         
         #'(make class
             [sym 'node-sym]
             rest-bindings ...)))

     (define/public (rest-constructor-bindings) #'())
     
     ))


 (define (root? %) (is-a? % root<%>))
 (define (leaf? %) (is-a? % leaf<%>))
 (define (intr? %) (is-a? % intr<%>))

 (define ancestor<%>
   (interface (node<%> base:ancestor<%>)
     [absorb-children (->m syntax? (listof (is-a?/c base:descendant<%>)))]
     [child-leaf% (->m leaf?)]
     [child-intr% (->m intr?)]
     ))

 (define ancestor-mixin
   (mixin (node<%> base:ancestor<%>) (ancestor<%>)
     (super-new)

     (inherit init-children!)

     (abstract child-leaf% child-intr%)
     
     (inherit-field sym children)
     
     (define/override (absorb stx)
       (init-children! (absorb-children (super absorb stx))))
     
     
     (define/public (absorb-children xs)
       (syntax-parse xs
         [() '()]
         
         [((x ...) xs ...)
          (define inter (new (child-intr%)))
          (send inter absorb #'(x ...))
          (cons inter (absorb-children #'(xs ...)))]
         
         [(x ...)
          (define leaf (new (child-leaf%)))

          (cons leaf (absorb-children (send leaf absorb #'(x ...))))]))


     (define/override (rest-constructor-bindings )
       (with-syntax ([(child-constructor ...) (map (λ (c) (send c produce-constructor-expr))
                                                   children)])
         #'([children (list child-constructor ...)])))
     
     
     ))



 
 (define leaf<%>
   (interface (node<%>)
     ))

 (define leaf-mixin
   (mixin (node<%>) (leaf<%>)
     (super-new)
     
     (define/override (produced-class-id) #'leaf-id)
     
     
     ))


 (define intr<%>
   (interface (ancestor<%>)
     ))


 (define intr-mixin
   (mixin (ancestor<%>) (intr<%>) 
     (super-new)

     (define/override (produced-class-id) #'intr-id)

     ))




 (define root<%>
   (interface (ancestor<%>)
     [root-binding [->m syntax?]]
     [produce-descendant-bindings (->m identifier? (listof syntax?))]
     [produce-annotation-expr (->m syntax?)]
     ))

 (define root-mixin
   (mixin (ancestor<%>) (root<%>)
     (super-new)
     
     (field [node-ht 'uninitialized])

     (inherit descendants produce-constructor-expr)
     
     (inherit-field sym)

     
     (define/public (root-binding)
       (with-syntax ([constructor (produce-constructor-expr)])
         #'(sym constructor)))


     (define/public (produce-descendant-bindings ht-id)
       (map (λ (node)
               (with-syntax ([name (introduce (get-field sym node))]
                             [ht-id ht-id])
                 #'(name (hash-ref ht-id 'name))))
            (descendants)))

     (define/override (produced-class-id) #'root-id)


     (define/public (produce-annotation-expr) #''())
     

     

     
     ))

 (define unannotated-ancestor-mixin
   (mixin (ancestor<%>) ()
     (super-new)

     (define/override (child-leaf%) AU-leaf%)
     (define/override (child-intr%) AU-intr%)

     ))




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
                     [(node-arg ...) (map (compose introduce
                                                   (field-getter sym))
                                          nodes)]
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
                  (send (root) descendants)) (syntax-local-introduce #'value)]
          [else #''value])))



     ))


 (define value-annotated-ancestor-mixin
   (mixin (annotated-node<%> ancestor<%>) ()
     (super-new)
     
     (define/override (child-leaf%) AVA-leaf%)
     (define/override (child-intr%) AVA-intr%)

     
     ))



 (define call-annotated-node-mixin
   (mixin (annotated-node<%>) ()
     (super-new)
     
     (inherit-field annotation-value)
     
     
     (define/override (absorb-annotation xs)
       (syntax-parse xs
         [((~datum <-) v:id nxs ...)
          (set! annotation-value #'v)
          #'(nxs ...)]
         [_ xs]))

     
     

     ))


 (define call-annotated-ancestor-mixin
   (mixin (annotated-node<%> ancestor<%>) ()
     (super-new)
     
     (define/override (child-leaf%) ACA-leaf%)
     (define/override (child-intr%) ACA-intr%)
     
     ))




 (define value-annotated-root-mixin
   (mixin (annotated-root<%>) ()
     (super-new)

     (inherit find-dfs)
     (inherit-field node-ht)

     (define/override (struct-constructor) #'value-annotation)


     ))


 (define call-annotated-root<%>
   (interface (annotated-root<%>)
     [produce-call-expr (->m syntax?)]))

 
 (define call-annotated-root-mixin
   (mixin (annotated-root<%>) ()
     (super-new)

     (field [call-node 'uninitialized])
     (inherit-field sym)
     
     (inherit annotated-nodes)

     (define/override (absorb stx)
       (super absorb stx)
       (define nodes (annotated-nodes))
       (match (length nodes)
         [1 (set! call-node (car nodes))]
         [0 (void)]
         [_ (raise-syntax-error 'bad-tree-syntax "multiple annotation for ACA tree'" stx)]))
     
     
     (define/public (produce-call-expr)
       (cond
        [(initialized? call-node)
         (with-syntax ([node (introduce (get-field sym call-node))]
                       [method (get-field annotation-value call-node)])
           #'(send node method))]
        [else #'(void)]))
     
     (define/override (struct-constructor) #'call-annotation)
     

     ))


 (define-composed-mixins
   [ancestor-sum (node ancestor)]
   
   [leaf-sum (base:leaf-sum node leaf)]
   [intr-sum (base:intr-sum ancestor-sum intr)]
   [root-sum (base:root-sum ancestor-sum root)]

   )


 (define AU-leaf% (class-from-mixins leaf-sum))
 (define AU-intr% (class-from-mixins intr-sum unannotated-ancestor))
 (define AU-root% (class-from-mixins root-sum unannotated-ancestor))

 (define ACA-leaf% (class-from-mixins leaf-sum annotated-node call-annotated-node))
 (define ACA-intr% (class-from-mixins intr-sum annotated-node call-annotated-node call-annotated-ancestor))
 (define ACA-root% (class-from-mixins root-sum
                                      annotated-node
                                      call-annotated-node
                                      call-annotated-ancestor
                                      annotated-root
                                      call-annotated-root))

 (define AVA-leaf% (class-from-mixins leaf-sum annotated-node value-annotated-node))
 (define AVA-intr% (class-from-mixins intr-sum annotated-node value-annotated-node value-annotated-ancestor))
 (define AVA-root% (class-from-mixins root-sum
                                      annotated-node
                                      value-annotated-node
                                      value-annotated-ancestor
                                      annotated-root
                                      value-annotated-root))



 (define (make-absorber % node-stx)
   (define absorber (new %))
   (send absorber absorb node-stx)
   absorber)
 
 (define-namespace-anchor a)
 (define absorber-ns (namespace-anchor->namespace a))
 
)




(define-syntax (with-nodes stx)
  (syntax-parse stx
    [(_ #:leaf leaf:expr
        #:intr intr:expr
        #:root root:expr
        body ...+)
     #'(let ([leaf% leaf]
             [intr% intr]
             [root% root])
         (syntax-parameterize
          ([leaf-id (syntax-id-rules () [_ leaf%])]
           [intr-id (syntax-id-rules () [_ intr%])]
           [root-id (syntax-id-rules () [_ root%])])
          body ...))]))


(define-syntax (make-tree stx)
  (syntax-parse stx
    [(_ (node ...))
     (send (make-absorber AU-root% #'(node ...)) produce-constructor-expr)
     ]))

(define-syntax (with-tree stx)
  
  (syntax-parse stx
    [(_ #:annotation annotation:id
        #:nodes (root:id desc ...)
        body ...+)

     (define root-absorber (make-absorber
                            (namespace-variable-value (string->symbol (format "~a-root%" (syntax-e #'annotation))) #t #f absorber-ns)
                            #'(root desc ...)))
     
     (with-syntax*
      ([constructor-expr (send root-absorber produce-constructor-expr)]
       [ht-id #'descendant-ht]
       [(descendant-binding ...) (send root-absorber produce-descendant-bindings #'ht-id)]
       [annotations-id (introduce 'annotations)]
       [annotation-expr (send root-absorber produce-annotation-expr)]
       )
      
      #'(let* ([root constructor-expr]
               [ht-id (send root make-ht (field-getter sym))]
               descendant-binding
               ...
               [annotations-id annotation-expr]
               )
          body
          ...))]))



(define-syntax (test-return-values stx)
  (syntax-parse stx
    [(_ ((~datum test)
         #:method method:id
         #:nodes (node ...))
        ...+)
     (with-syntax ([(test-case-title ...) (stx-map (compose symbol->string syntax-e) #'(method ...))]
                   [annotations (introduce 'annotations)])
       
       #'(begin
           (test-case
            test-case-title
            (with-tree #:annotation AVA
                       #:nodes (node ...)

                       (for ([ann annotations])
                         (define annotated-node (annotation-node ann))
                         (check-equal? (send annotated-node method)
                                       (value-annotation-value ann)
                                       (format "node name: ~a" (get-field sym annotated-node))))))
           ...))]))



(define-syntax (test-tree-state-modifications stx)
  (syntax-parse stx
    [(_
      #:name test-name:str
      (~optional (~seq #:root-initializer root-initializer:expr))
      (~optional (~seq #:key key:expr))
      (root-node:expr ...+)
      ...+)
     
     (define absorbers (stx-map (curry make-absorber ACA-root%) #'((root-node ...) ...)))


     (define make-root-initilizer
       (cond
        [(attribute root-initializer)
         (λ (root-id)
            (with-syntax ([root root-id])
              #'((root-initializer root))))]
        [else (const #'())]))
     
     
     (with-syntax*
      ([initial-constructor-expr (send (car absorbers) produce-constructor-expr)]
       [(state-constructor-expr ...) (map (method produce-constructor-expr) (cdr absorbers))]
       [ht-id #'descendant-ht]
       [(descendant-binding ...) (send (car absorbers) produce-descendant-bindings #'ht-id)]

       [current-state-id (datum->syntax stx 'current-state)]
       [(current-state-initializer ...) (make-root-initilizer #'current-state-id)]

       [expected-state-id (datum->syntax stx 'expected-state)]
       [(expected-state-initializer ...) (make-root-initilizer #'expected-state-id)]
       
       [(call-expr ...) (map (method produce-call-expr) (drop-right absorbers 1))]
       [(expected-constructor-expr ...) (map (method produce-constructor-expr) (cdr absorbers))]
       [check-expr (cond
                    [(attribute key) #'(check-equal? (key current-state-id) (key expected-state-id))]
                    [else #'(check-equal? current-state-id expected-state-id)])])

      
      #'(test-case
         test-name
         (let* ([current-state-id initial-constructor-expr]
                [ht-id (send current-state-id make-ht (field-getter sym))]
                descendant-binding ...)
           current-state-initializer
           ...
           (let ([expected-state-id expected-constructor-expr])
             expected-state-initializer
             ...
             call-expr
             check-expr)
           ...))

      
     )]))




;; (with-nodes
;;  #:leaf (symboled-node-mixin base:leaf%)
;;  #:intr (symboled-node-mixin base:intr%)
;;  #:root (symboled-node-mixin base:root%)

;;  (with-tree #:annotation AU
;;             ;#:name-key (compose string->path symbol->string)
;;             #:nodes (qwe asdf
;;                          ewq)
;;             'ok))

