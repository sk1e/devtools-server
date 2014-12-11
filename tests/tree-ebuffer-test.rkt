#lang racket/base

(require ss/racket/class
         racket/pretty
         
         rackunit
         rackunit/text-ui

         "../tree/ebuffer.rkt"
         "../tree/edsl/edsl.rkt"
         "../tree/edsl/utils.rkt"
         "../backend/buffer.rkt"
         "../backend/emacs.rkt"
         "../constants.rkt"
         )




(define ebuffer-root-refine-mixin
  (mixin (ebuffer:root<%>) ()
    (super-new)
    
    (field [test-tree-buffer (new emulated-buffer% [name "project-tree-buffer"])])
    
    ))


(define ebuffer-node-refine-mixin
  (mixin (ebuffer:node<%>) ()
    (super-new)

    
    (inherit root)
    (inherit-field sym)
    
    (define/override (get-name) (symbol->string sym))

    (define/override (tree-buffer) 
      (get-field test-tree-buffer (root)))
    
        
    ))


(define ebuffer-descendant-refine-mixin
  (mixin (ebuffer:descendant<%>) ()
    (super-new)
    
    (define/override (post-select!) (void))))

(define leaf% (class-from-mixins ebuffer:leaf-final-sum
                                 symboled-node
                                 symbol-inspected-node
                                 ebuffer-node-refine
                                 ebuffer-descendant-refine))

(define intr% (class-from-mixins ebuffer:intr-final-sum
                                 symboled-node
                                 symbol-inspected-node
                                 ebuffer-node-refine
                                 ebuffer-descendant-refine))

(define root% (class-from-mixins ebuffer:root-final-sum
                                 symboled-node
                                 symbol-inspected-node
                                 ebuffer-node-refine
                                 ebuffer-root-refine))

(with-nodes
 #:leaf leaf%
 #:intr intr%
 #:root root%


 (run-tests
  (test-suite
   "ebuffer tree test "


   (with-tree
    #:annotation AU
    #:nodes (root-0 leaf-1
                    leaf-2
                    (inter-3 leaf-31
                             leaf-32
                             leaf-33
                             (inter-34 (inter-341)))
                    leaf-4)
    (send root-0 init-tree-buffer!)

    (test-case
     "buffer content initialization"
     (check-equal? (get-field content (send root-0 tree-buffer))
                   (make-buffer-string ("leaf-1\n" 'font-lock-face 'pt:leaf-face) 
                                       ("leaf-2\n" 'font-lock-face 'pt:leaf-face)
                                       ("inter-3\n" 'font-lock-face 'pt:intr-face)
                                       ("    leaf-31\n" 'font-lock-face 'pt:leaf-face)
                                       ("    leaf-32\n" 'font-lock-face 'pt:leaf-face)
                                       ("    leaf-33\n" 'font-lock-face 'pt:leaf-face)
                                       ("    inter-34\n" 'font-lock-face 'pt:intr-face)
                                       ("        inter-341\n" 'font-lock-face 'pt:intr-face)
                                       ("leaf-4\n" 'font-lock-face 'pt:leaf-face))))

    
    (test-case
     "buffer content initialization"
     (for ([desc (send root-0 descendants)])
       
       (check-equal? (send+ root-0
                            (tree-buffer)
                            (substring-content (send desc resultant-point)
                                               (send desc resultant-solo-end-point)))
                     (send desc solo-representation))))

    (test-case
     "select!"
     (send leaf-2 select!)
     (check-equal? (get-field content (send root-0 tree-buffer))
                   (make-buffer-string ("leaf-1\n" 'font-lock-face 'pt:leaf-face) 
                                       ("leaf-2\n" 'font-lock-face `(:inherit pt:leaf-face :background ,const:selection-background-color))
                                       ("inter-3\n" 'font-lock-face 'pt:intr-face)
                                       ("    leaf-31\n" 'font-lock-face 'pt:leaf-face)
                                       ("    leaf-32\n" 'font-lock-face 'pt:leaf-face)
                                       ("    leaf-33\n" 'font-lock-face 'pt:leaf-face)
                                       ("    inter-34\n" 'font-lock-face 'pt:intr-face)
                                       ("        inter-341\n" 'font-lock-face 'pt:intr-face)
                                       ("leaf-4\n" 'font-lock-face 'pt:leaf-face))))

    
    
    ;; (test-case
    ;;  "remove-from-tree!"
    ;;  (send leaf-2 remove-from-tree!)
    ;;  (check-equal? (get-field content (send root-0 tree-buffer))
    ;;                (make-buffer-string ("leaf-1\n" 'font-lock-face `(:inherit pt:leaf-face :background ,const:selection-background-color)) 
    ;;                                    ("inter-3\n" 'font-lock-face 'pt:intr-face)
    ;;                                    ("    leaf-31\n" 'font-lock-face 'pt:leaf-face)
    ;;                                    ("    leaf-32\n" 'font-lock-face 'pt:leaf-face)
    ;;                                    ("    leaf-33\n" 'font-lock-face 'pt:leaf-face)
    ;;                                    ("    inter-34\n" 'font-lock-face 'pt:intr-face)
    ;;                                    ("        inter-341\n" 'font-lock-face 'pt:intr-face)
    ;;                                    ("leaf-4\n" 'font-lock-face 'pt:leaf-face))))

    
    ;; (send leaf-1 remove-from-tree!)
    ;; (check-equal? (get-field content (send root-0 tree-buffer))
    ;;               (make-buffer-string ("inter-3\n" 'font-lock-face 'pt:intr-face)
    ;;                                   ("    leaf-31\n" 'font-lock-face `(:inherit pt:leaf-face :background ,const:selection-background-color))
    ;;                                   ("    leaf-32\n" 'font-lock-face 'pt:leaf-face)
    ;;                                   ("    leaf-33\n" 'font-lock-face 'pt:leaf-face)
    ;;                                   ("    inter-34\n" 'font-lock-face 'pt:intr-face)
    ;;                                   ("        inter-341\n" 'font-lock-face 'pt:intr-face)
    ;;                                   ("leaf-4\n" 'font-lock-face 'pt:leaf-face)))

    
    ;; (send* inter-34 (select-as-new!) (remove-from-tree!))
    ;; (check-equal? (get-field content (send root-0 tree-buffer))
    ;;               (make-buffer-string ("inter-3\n" 'font-lock-face 'pt:intr-face)
    ;;                                   ("    leaf-31\n" 'font-lock-face 'pt:leaf-face)
    ;;                                   ("    leaf-32\n" 'font-lock-face 'pt:leaf-face)
    ;;                                   ("    leaf-33\n" 'font-lock-face `(:inherit pt:leaf-face :background ,const:selection-background-color))
    ;;                                   ("leaf-4\n" 'font-lock-face 'pt:leaf-face)))

    
    ;; (send* inter-3 (select-as-new!) (remove-from-tree!))
    ;; (check-equal? (get-field content (send root-0 tree-buffer))
    ;;               (make-buffer-string ("leaf-4\n" 'font-lock-face `(:inherit pt:leaf-face :background ,const:selection-background-color))))
    )


   ;; (send (get-field project-buffer (emacs)) clear-buffer!)
   ;; (displayln (send (get-field+ (emacs) project-buffer content) bs-no-properties))

   ;; (displayln "clear-subtree! -----")
   
   (test-tree-state-modifications
    #:name "clear-subtree!"
    #:root-initializer (method init-tree-buffer!)
    #:key (compose (field-getter content) (method tree-buffer))
    
    (root-0 leaf-1 
            leaf-2 
            (inter-3 leaf-31 
                     leaf-32 <- clear-subtree!
                     leaf-33)
            leaf-4)


    (root-0 leaf-1 
            leaf-2 
            (inter-3 <- clear-subtree!
                     leaf-31 
                     leaf-33)
            leaf-4)


    (root-0 leaf-1 
            leaf-2 <- clear-subtree!
            leaf-4)


    (root-0 leaf-1 
            leaf-4 <- clear-subtree!)

    (root-0 leaf-1)
    
    )

   

     
    (test-tree-state-modifications
     #:name "lift and lower"
     #:root-initializer (method init-tree-buffer!)
     #:key (compose (field-getter content) (method tree-buffer))
     
     (root-0 leaf-1 
             leaf-2 
             (inter-3 leaf-31 
                      leaf-32 <- lift-if-possible!
                      leaf-33))
     
     (root-0 leaf-1 
             leaf-2 
             (inter-3 leaf-32  <- lift-if-possible!
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
             (inter-3 <- lift-if-possible!
                      leaf-31 
                      leaf-33
                      leaf-32))
     
     (root-0 leaf-1 <- lower-if-possible!
             (inter-3 leaf-31 
                      leaf-33
                      leaf-32)
             leaf-2)


     (root-0 (inter-3 leaf-31 
                      leaf-33
                      leaf-32)
             leaf-1
             leaf-2)

     )
   
   )))

