#lang racket/base

(require racket/contract
         racket/list
         racket/function
         racket/match
         racket/file

         racket/class
         
         ss/racket/class
         ss/racket/provide
                  
         "base.rkt"
         "serialization.rkt"
         "../utils/path.rkt")


(provide (prefix-out file: (combine-out (suffixed-as interface mixin class
                                                     #:from (all-defined-out))
                                        new-descendant-from-path
                                        path->list)))


;; (define-namespace-anchor anchor)
;; (define ns (namespace-anchor->namespace anchor))
;; (eval '(new object%)
;;       ns)

;; (define/contract (ff l)
;;   (-> list? void?)
;;   (void))


(define (intr? v)       (is-a? v intr<%>))
(define (leaf? v)       (is-a? v leaf<%>))



(define node<%>
  (interface (base:node<%> serialization:node<%>)
    [absolute-path (->m path-string?)]
    [reversed-node-path-list (->m (listof path-string?))]
    [node-path-list (->m (listof path-string?))]
    [init-name! (->m path-string? void?)]

    
    file-root
    ))

(define node-mixin
  (mixin (base:node<%> serialization:node<%>) (node<%>)
    (super-new)

    
    (field [name 'uninitialized])

    (inherit node-identifier)
    
    (abstract absolute-path
              reversed-node-path-list
              file-root)

    (define/override (field-setter-exprs)
      (cons `(send ,(node-identifier) init-name!
                   ,(path->string name))
            (super field-setter-exprs)))
    
    (define/public (node-path-list) (reverse (reversed-node-path-list)))
    
    (define/public (init-name! v)
      (set! name (match v
                   [(? path? _) v]
                   [(? string? _) (string->path-element v)])))

    
    (define/override (repr-fields)
      (cons 'name (super repr-fields)))

    (define/override (equality-fields)
      (cons 'name (super equality-fields)))
    
    ))



(define (ancestor-implementation? %) (implementation? % ancestor<%>))
(define (intr-implementation? %) (implementation? % intr<%>))
(define (leaf-implementation? %) (implementation? % leaf<%>))


(define (ancestor? node) (is-a? node ancestor<%>))

(define descendant<%>
  (interface (base:descendant<%> node<%>)
    
    [init-ancestors! (->m path-string? void?)]
    [path-till [->m ancestor? path-string?]]
    [rename-file-or-directory! (->m path-string? void?)]
    [remove-file-or-directory! (->m void?)]
    ))

(define descendant-mixin
  (mixin (base:descendant<%> node<%>) (descendant<%>)
    (super-new)

    (inherit all-ancestors-till node-path-list)
    (inherit-field name parent)


    (define/public (path-till ancestor)
      (apply build-path (map (field-getter name)
                             (reverse (cons this
                                            (all-ancestors-till ancestor))))))
    
    (define/override (absolute-path)
      (path-till (file-root)))

   
    (define/override (reversed-node-path-list)
      (cons name (send parent reversed-node-path-list)))

    (define/public (ancestor-path? path)
      (define maybe-ancestor-path-list (path->list path))
      (define my-path-list (node-path-list))
      (define diff (- (length my-path-list) (length maybe-ancestor-path-list)))

      (cond
       [(<= diff 0) #f]
       [else (list-prefix? maybe-ancestor-path-list my-path-list)]))

    (define/public (rename-file-or-directory! new-name)
      (rename-file-or-directory (absolute-path)
                                (build-path (send parent absolute-path) new-name)))


    (define/public (remove-file-or-directory!)
      (delete-directory/files (absolute-path)))
      

    (define/public (init-ancestors! directory-path)
      (define-values (base name _) (split-path directory-path))
      (if base
          (begin (set! parent (make simple-directory% [name name]))
                 (send parent init-ancestors! base))
          (set! parent (make root% [name name])))
      (set-field! children parent (list this)))

    (define/override (file-root)
      (send parent file-root))
    
    ))


(define/contract (new-descendant-from-path % path)
  (-> (implementation?/c descendant<%>)
      path-string?
      (is-a?/c descendant<%>))
  (define-values (base name _) (split-path path))
  (define file (make % [name name]))
  (send file init-ancestors! base)
  file)



(define ancestor<%>
  (interface (base:ancestor<%> node<%>)
    [child-directory% (->m intr-implementation?)]
    [child-file%      (->m string? leaf-implementation?)]
    [find-by-path (->i ([self any/c]
                        [path (self) (λ (p) (send self descendant-path? p))])
                       
                       [res () (or/c #f (is-a?/c descendant<%>))])]
    [find-by-list (->m list? (or/c #f (is-a?/c descendant<%>)))]
    
    [descendant-path? (->m path-string? any)]
    ))

(define ancestor-mixin
  (mixin (base:ancestor<%> node<%>) (ancestor<%>)
    (super-new)

    (abstract child-directory%
              child-file%)
    
    (inherit-field children)
    (inherit depth node-path-list)

    
    (define/public (find-by-path path)
      (find-by-list (list-tail (path->list path)
                               (add1 (depth)))))

    (define/public (find-by-list lst)
      (match lst
        [(cons x xs) (let ([desc (findf (λ (obj) (equal? (get-field name obj) x))
                                        children)])
                       (cond
                        [(not desc) #f]
                        [(null? xs) desc]
                        [else (send desc find-by-list xs)]))]
        ['() this]))
    
    (define/public (descendant-path? path)
      (define maybe-desc-path-list (path->list path))
      (define my-path-list (node-path-list))
      (define diff (- (length maybe-desc-path-list) (length my-path-list)))
      
      (cond
       [(<= diff 0) #f]
       [else (list-prefix? my-path-list maybe-desc-path-list)]))

    
    
    ))




(define intr<%>
  (interface (ancestor<%> descendant<%>)
    
    [new-directory (->m string? intr?)]
    [new-file (->m string? leaf?)]
    [make-directory! (->m void?)]
    [make-directory-if-not! (->m void?)]
    
    fill-recursively
    ))

(define intr-mixin
  (mixin (descendant<%> ancestor<%>) (intr<%>)
    (super-new)
    
    (inherit-field children)
    
    (inherit absolute-path
             init-children!
             push-child!
             child-file%
             child-directory%)
    

    (define/public (fill-recursively #:filter-not-rx [filter-rx #rx""])
      (define acceptable-name? (negate (curry regexp-match? filter-rx)))

      (define-values (files dirs) (partition (compose file-exists? (curry build-path (absolute-path)))
                                             (directory-list (absolute-path))))

      
      (define dir-nodes (map (make-constructor (child-directory%) name)
                             (filter acceptable-name? dirs)))
      
      (init-children! (append (map (λ (file-name) (make (child-file% (path->string file-name))
                                                                  [name file-name]))
                                   (filter acceptable-name? files))
                              dir-nodes))
      
      (for-each (λ (node) (send node
                                fill-recursively
                                #:filter-not-rx filter-rx))
                dir-nodes))


    (define/public (new-file name)
      (make (child-file% name) [name name]))
    ;(make (child-file%) [name name]))
    
    (define/public (new-directory name)
      (make (child-directory%) [name name]))

    (define/public (make-directory!)
      (make-directory (absolute-path)))

    (define/public (make-directory-if-not!)
      (unless (directory-exists? (absolute-path))
        (make-directory!)))
    
    ))



(define leaf<%>
  (interface (descendant<%>)
    [make-file! (->m void?)]
    [make-file-if-not! (->m void?)]
    ))


(define leaf-mixin
  (mixin (descendant<%>) (leaf<%>)
    (super-new)
    
    (inherit absolute-path)

    (define/public (make-file!)
      (call-with-output-file (absolute-path) void))

    (define/public (make-file-if-not!)
      (unless (file-exists? (absolute-path))
        (make-file!)))
    
    ))





(define simple-specifier-mixin
  (mixin (ancestor<%>) ()
    (super-new)
    
    (define/override (child-file% name) simple-file%)
    (define/override (child-directory%) simple-directory%)
    
    ))


(define final-root-directory-mixin (compose simple-specifier-mixin
                                            ancestor-mixin
                                            node-mixin
                                            serialization:root-sum-mixin
                                            base:root-sum-mixin))




;; (define-serializable-class root% (final-root-directory-mixin object%)
;;     (super-new)

;;     (define/override (absolute-path)
;;       (string->path "/"))

;;     (define/override (reversed-node-path-list)
;;       (list (absolute-path)))

;;     (define/override (file-root) this)


;;     )

(define-inspected-class root%
  (class (final-root-directory-mixin object%)
    (super-new)

    (define/override (absolute-path)
      (string->path "/"))

    (define/override (reversed-node-path-list)
      (list (absolute-path)))

    (define/override (file-root) this)

    
    ))




(define-composed-mixins
  [leaf-sum (node descendant leaf)]
  [intr-sum (node descendant ancestor intr)])


(define-composed-mixins
  [leaf-final-sum (base:leaf-sum serialization:leaf-sum node descendant leaf)]
  [intr-final-sum (base:intr-sum serialization:intr-sum node descendant ancestor intr)])


;; (define-serializable-class simple-file% (leaf-final-sum-mixin object%)
;;   (super-new))


(define-inspected-class simple-file%
  (class (leaf-final-sum-mixin object%)
    (super-new)))


;; ;; (define-serializable-class simple-directory% (simple-specifier-mixin (intr-final-sum-mixin object%))
;; ;;   (super-new))


(define-inspected-class simple-directory%
  (class (simple-specifier-mixin (intr-final-sum-mixin object%))
    (super-new)))




;; (define/contract (list-prefix? lst prefix)
;;   (-> list? list? any)  
;;   (equal? (take-right (reverse lst) (length prefix))
;;           prefix))

;; (list-prefix? '(1 2) '(1))

