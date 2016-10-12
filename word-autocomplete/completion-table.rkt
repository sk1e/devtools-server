#lang typed/racket/base

(require racket/match
         racket/list)

(provide completion-table-append
         completion-table-remove-one
         (struct-out completion-data)
         (struct-out exn:fail:word-autocomplete)
         (struct-out exn:fail:word-autocomplete:no-suffix)
         (struct-out exn:fail:word-autocomplete:bad-entries-number))
                     
                    




(struct completion-data ([ht : Completion-table]
                         [entries-number : Natural])
        #:transparent)

(define-type Completion-table (HashTable Char completion-data))


(struct exn:fail:word-autocomplete exn:fail ())
(struct exn:fail:word-autocomplete:no-suffix exn:fail:word-autocomplete ([word : String]
                                                                         [suffix : String]))

(struct exn:fail:word-autocomplete:bad-entries-number exn:fail:word-autocomplete ([word : String]
                                                                                  [entries-number : Natural]))





(: completion-table-append (Completion-table (Listof String) -> Completion-table))
(define (completion-table-append ht strings)
  (for/fold : Completion-table ([completions-table ht])
            ([string (in-list strings)])
            
            (let loop : Completion-table ([chars : (Listof Char) (string->list string)]
                       [char-table completions-table])
              (match chars
                [(cons x xs)
                 (define data (hash-ref char-table x (lambda () (completion-data #hasheqv() 0))))
                 (hash-set char-table x
                           (cond
                            [(null? xs)
                             (struct-copy completion-data data
                                          [entries-number (add1 (completion-data-entries-number data))])]
                            [else
                             (struct-copy completion-data data
                                          [ht (loop xs (completion-data-ht data))])]))]

                ['() char-table]))))





(: completion-table-remove-one (Completion-table String -> Completion-table))
(define (completion-table-remove-one top-ct word)
  (let loop : Completion-table ([chars : (Listof Char) (string->list word)]                                
                                [temp-acc : (Listof Char) '()]
                                [update-ct : Completion-table top-ct]
                                [ct : Completion-table top-ct])
       (match chars
         [(cons x xs)
          (cond
           [(not (hash-has-key? ct x)) (raise (exn:fail:word-autocomplete:no-suffix
                                               (format "completion-table-remove: no suffix for word in completion-table\nword: ~a\nsuffix: ~a" word (list->string chars))
                                               (current-continuation-marks)
                                               word
                                               (list->string chars)))]
           [else
            (define data (hash-ref ct x))
            (match xs
              ['() (match (completion-data-entries-number data)
                     [1  (hash-remove update-ct (match temp-acc
                                                  ['() x]
                                                  [_ (last temp-acc)]))]
                     [_ (completion-table-update-entries update-ct (reverse (cons x temp-acc)) word)]
                     )]
              [_ (match (hash-count (completion-data-ht (hash-ref ct x)))
                   [1 (loop xs (cons x temp-acc) update-ct (completion-data-ht (hash-ref ct x)))]
                   [_ (completion-table-update update-ct
                                               (reverse (cons x temp-acc))
                                               (loop xs
                                                     '()
                                                     (completion-data-ht (hash-ref ct x))
                                                     (completion-data-ht (hash-ref ct x))))])])])]
         )))

(: completion-table-update (Completion-table (Listof Char) Completion-table -> Completion-table))
(define (completion-table-update ct update-chars new-ct)
  (let loop : Completion-table ([ct : Completion-table ct]
                                [chars : (Listof Char) update-chars])
       (match chars
         [(cons x xs) (hash-update ct x (lambda (data)
                                          (struct-copy completion-data data
                                                       [ht (loop (completion-data-ht (hash-ref ct x)) xs)])))]
         ['() new-ct])))

(: completion-table-update-entries (Completion-table (Listof Char) String -> Completion-table))
(define (completion-table-update-entries ct update-chars word)
  (let loop : Completion-table ([ct : Completion-table ct]
                                [chars : (Listof Char) update-chars])
       (match chars
         [(list x) (hash-update ct x (lambda (data) (match data
                                                 [(completion-data ht (? exact-positive-integer? entries-number)) (completion-data ht (sub1 entries-number))]
                                                 [(completion-data _ entries-number)
                                                  (raise (exn:fail:word-autocomplete:bad-entries-number
                                                          (format "completion-table-remove: bad entries number for word in completion-table\nword: ~a\nentries-number: ~a" word entries-number)
                                                          (current-continuation-marks)
                                                          word
                                                          entries-number))])
                                        ))]
         [(cons x xs) (hash-update ct x (lambda (data)
                                          (struct-copy completion-data data
                                                       [ht (loop (completion-data-ht (hash-ref ct x)) xs)])))])))


;; (struct Base-Node ([height : Integer]))

;; (struct (A) Node Base-Node ([value   : A]
;;                             [left    : (AVL-Node A)]
;;                             [right   : (AVL-Node A)]))


;; (struct Empty-node Base-Node ())

;; (define-type (AVL-Node A) (U (Node A) Empty-node))


;; (define-type (Insert A) (-> (AVL-Node A) A (Node A)))

;; (: make-root (All (A) A
;;                   (A A -> Boolean)
;;                   (A A -> Boolean)
;;                   -> (Values (Node A) (Insert A))))
;; (define (make-root root-value value<? value=?)
  
;;   (: insert (Insert A))
;;   (define (insert node v)
;;     (cond
;;      [(Empty-node? node) (Node 1 v (Empty-node 0) (Empty-node 0))]
;;      [else
;;       (cond 
;;        [(value=? v (Node-value node)) node]
       
;;        [(value<? v (Node-value node))
;;         (define new-left (insert (Node-left node) v))
;;         (define node+inserted (struct-copy Node node
;;                                            [left new-left]
;;                                            [height #:parent Base-Node (add1 (Base-Node-height new-left))]))
        
;;         (match (balance-factor node+inserted)
;;           [2 (define left (Node-left node+inserted))
;;              (cond
;;               [(Node? left)
;;                (rotate-right
;;                 (match (balance-factor left)
;;                   [-1 (update-height
;;                        (struct-copy Node
;;                                     node+inserted
;;                                     [left (rotate-left left)]))]
;;                   [_ node+inserted]))]
;;               [else (error 'er)])]
          
;;           [_ node+inserted])]
       
;;        [else
;;         (define new-right (insert (Node-right node) v))
;;         (define node+inserted (struct-copy Node node
;;                                            [right new-right]
;;                                            [height #:parent Base-Node (add1 (Base-Node-height new-right))]))
        
;;         (match (balance-factor node+inserted)
;;           [-2 (define right (Node-right node+inserted))
;;              (cond
;;               [(Node? right)
;;                (rotate-left
;;                 (match (balance-factor right)
;;                   [1 (update-height
;;                        (struct-copy Node
;;                                     node+inserted
;;                                     [right (rotate-right right)]))]
;;                   [_ node+inserted]))]
;;               [else (error 'er)])]
          
;;           [_ node+inserted])])]))

  

;;   (: balance-factor : (Node A) -> Integer)
;;   (define (balance-factor node)
;;     (- (Base-Node-height (Node-left node))
;;        (Base-Node-height (Node-right node))))
  

  
;;   (: update-height : (Node A) -> (Node A))
;;   (define (update-height node)
;;     (struct-copy Node
;;                  node
;;                  [height #:parent Base-Node (add1 (max (Base-Node-height (Node-left node))
;;                                                        (Base-Node-height (Node-right node))))]))
  

  
;;   (: rotate-right : (Node A) -> (Node A))
;;   (define (rotate-right node)
;;     (define left (Node-left node))
;;     (cond
;;      [(Node? left)
;;       (define new-right (update-height
;;                          (struct-copy Node
;;                                       node
;;                                       [left (Node-right left)])))
;;       (update-height
;;        (struct-copy Node
;;                     (Node-left node)
;;                     [right new-right]))]
;;      [else (error 'unexpected-empty-node)]))
  

  
;;   (: rotate-left : (Node A) -> (Node A))
;;   (define (rotate-left node)
;;     (define right (Node-right node))
;;     (cond
;;      [(Node? right)
;;       (define new-left (update-height
;;                         (struct-copy Node
;;                                      node
;;                                      [right (Node-left right)])))
;;       (update-height
;;        (struct-copy Node
;;                     (Node-right node)
;;                     [left new-left]))]
;;      [else (error 'unexpected-empty-node)]))
  
;;   (values (Node 1 root-value (Empty-node 0) (Empty-node 0))
;;           insert))




;; (: repr-tree (All (A) (AVL-Node A) -> String))
;; (define (repr-tree node)
;;   (let repr ([node node]
;;              [depth 1])
;;     (define indent (make-string (* 2 depth) #\ ))
;;     (cond
;;      [(Node? node) (format "value: ~a height: ~a\n~aleft:  ~a\n~aright: ~a"
;;                            (~a (Node-value node) #:min-width 5)
;;                            (Base-Node-height node)
;;                            indent
;;                            (repr (Node-left node) (add1 depth))
;;                            indent
;;                            (repr (Node-right node) (add1 depth)))]
;;      [else (~a (Base-Node-height node))])))


;; ;; (define display-tree (compose displayln repr-tree))

;; (define-values (root insert) ((inst make-root Integer) 0 < =))

;; ;; (displayln ((inst repr-tree Integer) (foldl (λ ([a : Integer]
;; ;;                                                 [res : (Node Integer)])
;; ;;                                             (insert res a))
;; ;;                                          root
;; ;;                                          (range 100))))

;; ;; (for ([n (range 0 100)])
;; ;;   (displayln n))

;; ;; (define t0 (current-inexact-milliseconds))
;; ;; (foldl (λ ([a : Integer]
;; ;;            [res : (Node Integer)])
;; ;;           (insert res a))
;; ;;        root
;; ;;        (range 1 1000000))

;; ;; (displayln (/ (- (current-inexact-milliseconds) t0) 1000))


;; ;; (foldl (λ ([a : Exact-Nonnegative-Integer]
;; ;;            [res : (Node Exact-Nonnegative-Integer)])
;; ;;           (insert res a))
;; ;;        root
;; ;;        (range 1 50000))

