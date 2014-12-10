#lang racket/base

(require ss/racket/class
         
         rackunit
         rackunit/text-ui

         "../tree/base.rkt"
         "../tree/edsl/edsl.rkt"
         "../tree/edsl/utils.rkt"

         )




;; (define leaf% (named-node-mixin (symboled-node-mixin base:leaf%)))
;; (define intr% (named-node-mixin (symboled-node-mixin base:intr%)))
;; (define root% (named-node-mixin (symboled-node-mixin base:root%)))

(define leaf% (symbol-inspected-node-mixin (symboled-node-mixin base:leaf%)))
(define intr% (symbol-inspected-node-mixin (symboled-node-mixin base:intr%)))
(define root% (symbol-inspected-node-mixin (symboled-node-mixin base:root%)))

;; (define-tree-transformers leaf% intr% root%)

(with-nodes
 #:leaf leaf%
 #:intr intr%
 #:root root%

 (run-tests
  (test-suite
   "edsl test"
   
   (test-case
    "sym absorbing test"
    
    (with-tree #:annotation AU
               #:nodes (root-0 leaf-1
                               leaf-2
                               (intr-3 leaf-31
                                       leaf-32)
                               leaf-4)
               
               (check-equal? root-0
                             (make root%
                               [sym 'root-0]
                               [children (list (make leaf% [sym 'leaf-1])
                                               (make leaf% [sym 'leaf-2])
                                               (make intr%
                                                 [sym 'intr-3]
                                                 [children (list (make leaf% [sym 'leaf-31])
                                                                 (make leaf% [sym 'leaf-32]))])
                                               (make leaf% [sym 'leaf-4]))]))))
   
   

   
   (test-case
    "single annotation test"
    (with-tree #:annotation AVA
               #:nodes (root-0 leaf-1
                               leaf-2 -> 'value
                               (inter-3 leaf-31
                                        leaf-32
                                        leaf-33))
               (check-equal? annotations (list (make-value-annotation leaf-2 'value)))
               ))

   
   (test-case
    "AVA tree test for node substitution and DFS order"
    (with-tree #:annotation AVA
               #:nodes (root-0 leaf-1
                               leaf-2 -> 1
                               (inter-3 leaf-31 -> leaf-1
                                        leaf-32
                                        leaf-33)
                               (inter-4  -> 4
                                         (inter-5 -> 5)))
               (check-equal? annotations
                             (list (make-value-annotation leaf-2 1)
                                   (make-value-annotation leaf-31 leaf-1)
                                   (make-value-annotation inter-4 4)
                                   (make-value-annotation inter-5 5)))))


   
   
   )))



