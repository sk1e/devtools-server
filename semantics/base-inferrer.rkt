#lang racket/base

(require racket/contract
         racket/function
         racket/list
         racket/match
         racket/set
         racket/unit
         racket/port
         racket/format
         
         ss/racket/class
         
         "../abc-tree.rkt"
         "../file-tree.rkt"
         "../syntax/syntax-tree.rkt"
         "../syntax/grammar.rkt"
         "../final-tree.rkt"
         "../constants.rkt")


(define racket-base-path (build-path const:racket-path "collects" "racket" "private" "base.rkt"))

(define base-inferrer-interface
  (interface (directory-interface)
    ))


;; (define final-base-inferrer-mixin (compose directory-sum-mixin
;;                                            intermediate-sum-mixin))

(define directory%
  (class simple-directory%
    (super-new)

    (define/override (file%) inferrable-module%)))

(define (perform-inference)
  (define base-module (new-file-from-path inferrable-module% directory% racket-base-path))
  (send base-module init-module-content)
  ;(send base-module dependencies null)
  (for ([d (send base-module dependencies null)])
    (displayln (get-field name d)))
  'ok
  ;;(define imports (filter string? (send base-module all-imports)))
  
  
  )

;; (define base-inferrer%
;;   (class* (final-base-inferrer-mixin object%) (base-inferrer-interface)
;;     (super-new)

;;     (inherit-field children)
;;     (inherit fill descendants)

;;     (define/public (perform-inference)
;;       ;; (fill #:file-predicate (curry regexp-match? #rx"[.]rkt$"))

;;       ;; (for-each (method init-module-content) (modules))

      
      
;;       )
    
;;     (define/public (modules)
;;       (filter (make-is-a inferrable-module-interface)
;;               (descendants)))

    
;;     (define/override (file%) inferrable-module%)
    
;;     ))


(define inferrable-module-interface
  (interface ()
    ))


(define final-inferable-module-mixin (compose file-sum-mixin
                                              descendant-mixin
                                              syntax-module-sum-mixin
                                              ancestor-sum-mixin))




(define inferrable-module%
  (class* (final-inferable-module-mixin object%) (inferrable-module-interface)
    
    (field [require-modules 'uninitialized]
           [provide-symbols 'uninitialized])

    (inherit-field parent)
    
    (inherit absolute-path
             init-children
             descendants
             root)
    
    (super-new)


    (define/public (init-module-content)
      (init-children (call-with-input-file (absolute-path) parse-module-cells)))

    
    (define/public (successor?)
      ;(null? (filter #%require-form? (descendants)))
      (error 'not-implemented))

                                       
    (define/public (all-imports)
      (apply append (map (field-getter children)
                         (filter (curryr (method form?) "#%require")
                                 (descendants)))))
    
    (define/public (dependencies module-acc)
      (displayln (length module-acc))
      (define imports (map (method value)
                           (filter (make-is-a string%) (all-imports))))
      (if (null? imports)
          module-acc
          ;(let*-values [(module-names) (map (field-getter name) module-acc)])
          (let ([module-names (map (field-getter name) module-acc)])
            
            
            (define-values (initialized new) (partition (compose (curryr member module-names) string->path)
                                                        imports))
            
            (define new-paths (map (compose simplify-path
                                            (curry build-path (send parent absolute-path)))
                                   new))

            (define new-path-ht (let make-ht ([paths new-paths]
                                              [ht #hash()])
                                  (match paths
                                    [(cons x xs) (let-values ([(base name _) (split-path (car paths))])
                                                   (make-ht (cdr paths)
                                                            (hash-update ht base (curry cons name) '())))]
                                    ['() ht])))

            (define root-node (root))

            
            (define new-modules (apply append
                                       (hash-map new-path-ht
                                                 (λ (parent-path module-names)
                                                    (define modules (map (make-constructor this% name)
                                                                         module-names))
                                                    
                                                    (send (send root-node find-by-path parent-path)
                                                          add-children modules)
                                                    
                                                    modules))))
            (displayln "---")
            (for ((p new))
              (displayln p))
            (displayln "====")
            (for ((p initialized))
              (displayln p))
            
            (for-each (method init-module-content) new-modules)
            
            (foldl (λ (x res) (set-union (send x dependencies res) res))
                   (append module-acc new-modules)
                   new-modules))))
      
      

    
    (define/override (directory%) simple-directory%)

    (define/override (repr-fields) (cons 'name (super repr-fields)))
    
    ))


;; (set-intersect (list 1 2) (list 2 3))
;; (set-union (list 1 2) (list 2 3))
;; (match children
;;           [(cons x xs) (descs xs (if (is-a? x ancestor-interface)
;;                                      (append (reverse (send x descendants)) (list x) acc)
;;                                      (cons x acc)))]
;;           ['() (reverse acc)])
      
;; (define base-inferrer-mixin
;;   (mixin (directory-interface) (base-inferrer-interface)
;;     (super-new)
;;     ))



;; (define (infer-base-symbols path)
;;   (define inferrer (new-directory-from-path base-inferrer% path))
;;   (send inferrer perform-inference))


;(infer-base-symbols "/home/sergeek/racket/collects/racket/private/")
(perform-inference)
