#lang racket/base

(require racket/function
         
         ss/racket/class
         
         rackunit
         rackunit/text-ui

         "../tree/base.rkt"
         "../tree/edsl/edsl.rkt"
         "../tree/edsl/utils.rkt"

         )


;(provide (prefix-out qwe: leaff%))

;; (define-namespace-anchor a)
;; (define ns (namespace-anchor->namespace a))


;;(define leaff% (symbol-inspected-node-mixin (symboled-node-mixin base:leaf%)))


;(send (new cls+i) inf)

;; (class-info cls%)
;(class-info cls+i)

;; (define ff-mixin
;;   (mixin () ()
;;     (inspect #f)
;;     (super-new)))

;; (define leaff%
;;   (class (base:pprintable-node-mixin object%)
;;     ;(base:ff-mixin object%)
;;     ;; base:leaf%
;;     ;;(symbol-inspected-node-mixin (symboled-node-mixin base:leaf%))
;;     (inspect #f)
;;     (super-new)
    
;;     (define/public (infoe) (class-info this%))))

;; ;; ;(call-with-values (λ () (class-info leaff%)) (λ (a1 a2 a3 a4 a5 a6 a7) (displayln a1)))
;; ;; ;; (class-info leaff%)

;; (send (new leaff%) infoe)
;; ;(class-info leaff%)


(with-nodes
 #:leaf (symbol-inspected-node-mixin (symboled-node-mixin base:leaf%))
 #:intr (symbol-inspected-node-mixin (symboled-node-mixin base:intr%))
 #:root (symbol-inspected-node-mixin (symboled-node-mixin base:root%))

 ;(displayln (send (new leaff%) serialize))
 ;(send (new leaff%) get-class-name)
 
;; (define r (make root-id
;;             [children (list (make leaf-id))]))

;; ;(displayln r)
;; (send r serialize)
;; (displayln (send r serialize))

;; (displayln (eval (send r serialize) ns))

;(send r write "/home/god/wtest")
;; (displayln (make #<class:.../tree/edsl/utils.rkt:33:2>
;;              (children (list (make #<class:.../tree/edsl/utils.rkt:33:2>)))))

;; (displayln (eval '(make |#<class:.../tree/edsl/utils.rkt:33:2>|
;;                     (children (list (make |#<class:.../tree/edsl/utils.rkt:33:2>|))))))

;(read-node "/home/god/wtest")

(run-tests
 (test-suite
  "base tree test"
  (test-return-values

   (test #:method nodes
         #:nodes (root-0 leaf-1 -> (list leaf-1)
                         leaf-2 
                         (inter-3 -> (list inter-3 leaf-31 leaf-32 leaf-33)
                                  leaf-31 
                                  leaf-32 
                                  leaf-33)))

   (test #:method descendants
         #:nodes (root-0 -> (list leaf-1 leaf-2
                                  inter-3 leaf-31 leaf-32 leaf-33
                                  leaf-4)
                         leaf-1
                         leaf-2 
                         (inter-3 leaf-31 
                                  leaf-32 
                                  leaf-33)
                         leaf-4))

   
   (test #:method next-sibling
         #:nodes (root-0 leaf-1 -> leaf-2
                         leaf-2 -> inter-3
                         (inter-3 -> #f
                                  leaf-31 -> leaf-32
                                  leaf-32 -> leaf-33
                                  leaf-33 -> #f)))

   (test #:method prev-sibling
         #:nodes (root-0 leaf-1 -> #f
                         leaf-2 -> leaf-1
                         (inter-3 -> leaf-2
                                  leaf-31 -> #f
                                  leaf-32 -> leaf-31
                                  leaf-33 -> leaf-32
                                  )))

   (test #:method next-descendant-neighbour
         #:nodes (root-0 leaf-1 -> leaf-2
                         leaf-2 -> inter-3
                         (inter-3 leaf-31 -> leaf-32
                                  leaf-32 
                                  leaf-33 -> inter-4)
                         (inter-4 -> #f
                                  (inter-5 -> #f))))

   
   (test #:method next-neighbour
         #:nodes (root-0 leaf-1 -> leaf-2
                         leaf-2 -> inter-3
                         (inter-3 -> leaf-31
                                  leaf-31 -> leaf-32
                                  leaf-32 
                                  leaf-33 -> inter-4)
                         (inter-4 -> inter-5
                                  (inter-5 -> #f))))
   

   
   
   (test #:method last-descendant-or-self
         #:nodes (root-0 leaf-1 
                         leaf-2 
                         (inter-3 -> inter-34
                                  leaf-31 
                                  leaf-32 
                                  leaf-33
                                  (inter-34) -> inter-34)
                         (inter-4 -> inter-5
                                  (inter-5))))

   
   
   (test #:method prev-neighbour
         #:nodes (root-0 leaf-1 -> #f
                         leaf-2 -> leaf-1
                         (inter-3 -> leaf-2
                                  leaf-31 -> inter-3
                                  leaf-32 
                                  leaf-33 -> leaf-32
                                  (inter-34))
                         (inter-4 -> inter-34 
                                  (inter-5 -> inter-4))))

   (test #:method next-intr
         #:nodes (root-0 leaf-1 -> inter-3
                         leaf-2 -> inter-3
                         (inter-3 -> inter-4
                                  leaf-31 -> inter-4
                                  leaf-32 
                                  leaf-33)
                         (inter-4 -> inter-5
                                  (inter-5 -> #f))))


   (test #:method circular-next-intr
         #:nodes (root-0 leaf-1 -> inter-3
                         leaf-2 -> inter-3
                         (inter-3 -> inter-4
                                  leaf-31 -> inter-4
                                  leaf-32 
                                  leaf-33)
                         (inter-4 -> inter-5
                                  (inter-5 -> inter-3))))


   
   (test #:method prev-intr
         #:nodes (root-0 leaf-1 -> #f
                         leaf-2
                         (inter-3 -> #f
                                  leaf-31 -> inter-3
                                  leaf-32 
                                  leaf-33)
                         (inter-4 -> inter-3
                                  (inter-5 -> inter-4))))


   (test #:method circular-prev-intr
         #:nodes (root-0 leaf-1 -> inter-5
                         leaf-2
                         (inter-3 -> inter-5
                                  leaf-31 -> inter-3
                                  leaf-32 
                                  leaf-33)
                         (inter-4 -> inter-3
                                  (inter-5 -> inter-4))))
   
   
   (test #:method first-intr
         #:nodes (root-0 -> inter-3
                         leaf-1
                         leaf-2
                         (inter-3 -> #f
                                  leaf-31 
                                  leaf-32 
                                  leaf-33)
                         (inter-4 -> inter-5
                                  (inter-5 -> #f))))
   


   (test #:method last-intr
         #:nodes (root-0 -> inter-5
                         leaf-1
                         leaf-2
                         (inter-3 -> #f
                                  leaf-31 
                                  leaf-32 
                                  leaf-33)
                         (inter-4 -> inter-5
                                  (inter-5 -> #f))))
   


   
  
   (test #:method next-leaf
         #:nodes (root-0 leaf-1 -> leaf-2
                         leaf-2 -> leaf-31
                         (inter-3 -> leaf-31
                                  leaf-31 -> leaf-32
                                  leaf-32 
                                  leaf-33 -> #f)
                         (inter-4 
                          (inter-5 -> #f))))
   



   
   (test #:method circular-next-leaf
         #:nodes (root-0 leaf-1 -> leaf-2
                         leaf-2 -> leaf-31
                         (inter-3 -> leaf-31
                                  leaf-31 -> leaf-32
                                  leaf-32 
                                  leaf-33 -> leaf-1)
                         (inter-4 
                          (inter-5 -> leaf-1))))


   
   (test #:method prev-leaf
         #:nodes (root-0 leaf-1 -> #f
                         leaf-2 -> leaf-1
                         (inter-3 -> leaf-2
                                  leaf-31 -> leaf-2
                                  leaf-32 
                                  leaf-33 -> leaf-32)
                                  (inter-4 
                                   (inter-5 -> leaf-33))))
   


   
   (test #:method circular-prev-leaf
         #:nodes (root-0 leaf-1 -> leaf-33
                         leaf-2 -> leaf-1
                         (inter-3 -> leaf-2
                                  leaf-31 -> leaf-2
                                  leaf-32 
                                  leaf-33 -> leaf-32)
                                  (inter-4 
                                   (inter-5 -> leaf-33))))
   



   
   (test #:method first-leaf
         #:nodes (root-0 -> leaf-1
                         leaf-1 -> leaf-1
                         leaf-2 
                         (inter-3 -> leaf-341
                                  (inter-34 -> leaf-341
                                            leaf-341 
                                            leaf-342 
                                            leaf-343))
                         (inter-4 -> leaf-42
                                  (inter-41 -> #f)
                                  leaf-42)))
   

   
   (test #:method last-leaf
         #:nodes (root-0 -> leaf-343
                         leaf-1 -> leaf-1
                         leaf-2 
                         (inter-3 -> leaf-343
                                  (inter-34 -> leaf-343
                                            leaf-341 
                                            leaf-342 
                                            leaf-343))
                         (inter-4 -> #f
                                  (inter-5 -> #f)))))
  

  
  (test-tree-state-modifications
   #:name "lift and lower"
   
   (root-0 leaf-1 
           leaf-2 
           (inter-3 leaf-31 
                    leaf-32 <- lift-if-possible!
                    leaf-33))

   (root-0 leaf-1 
           leaf-2 
           (inter-3 leaf-32 <- lift-if-possible!
                    leaf-31 
                    leaf-33))
   
   (root-0 leaf-1 
           leaf-2 
           (inter-3 leaf-32 <- lower-if-possible!
                    leaf-31 
                    leaf-33))
   
   (root-0 leaf-1 
           leaf-2 
           (inter-3 leaf-31
                    leaf-32 <- lower-if-possible!
                    leaf-33))


   (root-0 leaf-1 
           leaf-2 
           (inter-3 leaf-31 
                    leaf-33
                    leaf-32 <- lower-if-possible!))

   (root-0 leaf-1 
           leaf-2 
           (inter-3 leaf-31
                    leaf-33
                    leaf-32))
     
    
     )

  (test-tree-state-modifications
   #:name "remove!"
   
   (root-0 leaf-1 
           leaf-2 
           (inter-3 leaf-31 
                    leaf-32 <- remove!
                    leaf-33))

   (root-0 leaf-1 
           leaf-2 
           (inter-3 <- remove! 
                    leaf-31
                    leaf-33))

   (root-0 leaf-1 
           leaf-2 <- remove!)
   
   (root-0 leaf-1 <- remove!)

   (root-0))

  
  (test-case
   "find-dfs"
   (with-tree
    #:annotation AU
    #:nodes (root-0 leaf-1
                    leaf-2
                    (inter-3 leaf-31
                             leaf-32
                             leaf-33
                             (inter-34 (inter-341)))
                    leaf-4)
    

    (define obj (curry curry eq?))
    (check-equal? (send root-0 find-dfs (obj leaf-2)) leaf-2)
    (check-equal? (send root-0 find-dfs (obj leaf-32)) leaf-32)
    (check-equal? (send root-0 find-dfs (obj inter-3)) inter-3)
    (check-equal? (send root-0 find-dfs (obj inter-34)) inter-34)))
  
  

  
  )))



