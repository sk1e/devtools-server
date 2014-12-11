#lang racket/base

(require racket/file
         racket/function
         
         ss/racket/class
         
         rackunit
         rackunit/text-ui
         
         "../tree/project.rkt"
         "../tree/file.rkt"
         "../backend/buffer.rkt"
         
         "../tree/edsl/edsl.rkt"
         "../tree/edsl/utils.rkt"
         
         "../constants.rkt")




(define project-root-refine-mixin
  (mixin (project:root<%>) ()
    (super-new)
    
    (field [test-tree-buffer (new emulated-buffer% [name "project-tree-buffer"])])
    
    ))






(define project-node-refine-mixin
  (mixin (project:node<%>) ()
    (super-new)
    
    (inherit root)
    
    (define/override (tree-buffer) (get-field test-tree-buffer (root)))
    
    
    ))


(define node-test-sum-mixin (compose-mixin symboled-node file-name-refine project-node-refine))


(with-nodes
 #:leaf (node-test-sum-mixin project:module%)
 #:intr (node-test-sum-mixin project:directory%)
 #:root (project-root-refine-mixin (node-test-sum-mixin project:root%))

 (run-tests
  (test-suite
   "project tree test"
   (after
    

    (make-os-file-tree "/home/god/"
                       '((test-dir (dir-1 file-11
                                          file-12.rkt)
                                   (dir-2 (dir-21 file-211)
                                          file-22)
                                   file-3
                                   file-4)))

    (define projects-node (file:new-descendant-from-path project:projects-directory% "/home/god/"))

    (send projects-node new-project-from-existing-dir! "test-dir")
    (define current-project (get-field current-project projects-node))

    (define (current-node) (get-field current-node current-project))
    (define (current-buffer-content) (get-field content (send current-project tree-buffer)))
    
    ;; (test-case
    ;;  "project-cache"
    ;;  (send current-project cache-project!)
    ;;  (check-equal? current-project
    ;;                (send projects-node read-project "test-dir")))
    
    (define node-ht (send current-project make-ht (compose string->symbol path->string (field-getter name))))
    (define node-ref (curry hash-ref node-ht))


    
    ;; (with-tree
    ;;  #:annotation AU
    ;;  #:nodes (test-dir (dir-1 file-11
    ;;                           file-12)
    ;;                    (dir-2 (dir-21 file-211)
    ;;                           file-22)
    ;;                    file-3
    ;;                    file-4)

    ;;  (test-equal? "node structure" current-project test-dir))


    
    (define empty-margin-value '((margin right-margin) (eval indicator-empty-Black)))
    (define empty-margin (make-buffer-string (" " 'display empty-margin-value)))
    (define gray-test-margin (make-buffer-string (" " 'display '((margin right-margin) (eval indicator-test-DarkGray)))))
    (define red-test-margin (make-buffer-string (" " 'display '((margin right-margin) (eval indicator-test-DarkRed)))))

    (define current-leaf-value `(:inherit pt:leaf-face :background ,const:selection-background-color))
    (define exec-leaf-value `(:inherit pt:leaf-face :foreground ,const:execution-foreground-color))
    (define current+exec-leaf-value `(:inherit pt:leaf-face
                                               :foreground ,const:execution-foreground-color
                                               :background ,const:selection-background-color))
    (define current-intr-value `(:inherit pt:intr-face :background ,const:selection-background-color))
    (define em-prefix (make-buffer-string empty-margin empty-margin))
    
    (test-case
     "buffer structture"

     
     (check-equal? (current-buffer-content)
                   (make-buffer-string ("dir-1\n" 'font-lock-face 'pt:intr-face)
                                       em-prefix ("    file-11\n" 'font-lock-face current-leaf-value)
                                       em-prefix ("    file-12.rkt\n" 'font-lock-face 'pt:leaf-face)
                                       ("dir-2\n" 'font-lock-face 'pt:intr-face)
                                       ("    dir-21\n" 'font-lock-face 'pt:intr-face)
                                       em-prefix ("        file-211\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("    file-22\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("file-3\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("file-4\n" 'font-lock-face 'pt:leaf-face)))
     )
    

    (test-case
     "indicator switch"
     (define file-22 (node-ref 'file-22))
     (after
      (send file-22 switch-on-indicator! 0)
      (check-equal? (send+ current-project
                           (tree-buffer)
                           (substring-content (send file-22 resultant-point)
                                              (send file-22 resultant-subtree-end-point)))
                    (make-buffer-string (" " 'display '((margin right-margin) (eval indicator-modified-DarkRed)))
                                        empty-margin
                                        ("    file-22\n" 'font-lock-face 'pt:leaf-face)))
      (send file-22 switch-off-indicator! 0)))


    (test-case
     "add-directory!"

     (send (current-node) add-directory! "new-dir")
     
     (check-equal? (current-buffer-content)
                   (make-buffer-string ("dir-1\n" 'font-lock-face 'pt:intr-face)
                                       ("    new-dir\n" 'font-lock-face current-intr-value)
                                       em-prefix ("    file-11\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("    file-12.rkt\n" 'font-lock-face 'pt:leaf-face)
                                       ("dir-2\n" 'font-lock-face 'pt:intr-face)
                                       ("    dir-21\n" 'font-lock-face 'pt:intr-face)
                                       em-prefix ("        file-211\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("    file-22\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("file-3\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("file-4\n" 'font-lock-face 'pt:leaf-face))))
    
    (test-case
     "add-file!"

     (send (current-node) add-file! "new-file")
     
     (check-equal? (current-buffer-content)
                   (make-buffer-string ("dir-1\n" 'font-lock-face 'pt:intr-face)
                                       ("    new-dir\n" 'font-lock-face 'pt:intr-face)
                                       em-prefix ("        new-file\n" 'font-lock-face current-leaf-value)
                                       em-prefix ("    file-11\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("    file-12.rkt\n" 'font-lock-face 'pt:leaf-face)
                                       ("dir-2\n" 'font-lock-face 'pt:intr-face)
                                       ("    dir-21\n" 'font-lock-face 'pt:intr-face)
                                       em-prefix ("        file-211\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("    file-22\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("file-3\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("file-4\n" 'font-lock-face 'pt:leaf-face))))
    
    (test-case
     "rename-desc!"
     (send (current-node) rename-desc! "new-renamed-file")
     (check-equal? (current-buffer-content)
                   (make-buffer-string ("dir-1\n" 'font-lock-face 'pt:intr-face)
                                 ("    new-dir\n" 'font-lock-face 'pt:intr-face)
                                 em-prefix ("        new-renamed-file\n" 'font-lock-face current-leaf-value)
                                 em-prefix ("    file-11\n" 'font-lock-face 'pt:leaf-face)
                                 em-prefix ("    file-12.rkt\n" 'font-lock-face 'pt:leaf-face)
                                 ("dir-2\n" 'font-lock-face 'pt:intr-face)
                                 ("    dir-21\n" 'font-lock-face 'pt:intr-face)
                                 em-prefix ("        file-211\n" 'font-lock-face 'pt:leaf-face)
                                 em-prefix ("    file-22\n" 'font-lock-face 'pt:leaf-face)
                                 em-prefix ("file-3\n" 'font-lock-face 'pt:leaf-face)
                                 em-prefix ("file-4\n" 'font-lock-face 'pt:leaf-face)))

    
    
    
     (send (current-node) select-prev-intr!)

     (send (current-node) rename-desc! "new-renamed-dir")
     (check-equal? (current-buffer-content)
                   (make-buffer-string ("dir-1\n" 'font-lock-face 'pt:intr-face)
                                       ("    new-renamed-dir\n" 'font-lock-face current-intr-value)
                                       em-prefix ("        new-renamed-file\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("    file-11\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("    file-12.rkt\n" 'font-lock-face 'pt:leaf-face)
                                       ("dir-2\n" 'font-lock-face 'pt:intr-face)
                                       ("    dir-21\n" 'font-lock-face 'pt:intr-face)
                                       em-prefix ("        file-211\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("    file-22\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("file-3\n" 'font-lock-face 'pt:leaf-face)
                                       em-prefix ("file-4\n" 'font-lock-face 'pt:leaf-face))))

    
    (test-case
     "test-name"
     (check-equal? (send (node-ref 'file-12.rkt) test-name)
                   "dir-1-file-12-test.rkt"))



    (let ([make-buffer-content (λ (file-12-test-bs file-12-bs)
                                  ;; file-12-test-bs without indicator prefix
                                  ;; file-12-bs with test prefix
                                  (send (make-buffer-string ("tests\n" 'font-lock-face 'pt:intr-face))
                                        concat
                                        em-prefix
                                        file-12-test-bs
                                        (make-buffer-string ("dir-1\n" 'font-lock-face 'pt:intr-face)
                                                            ("    new-renamed-dir\n" 'font-lock-face 'pt:intr-face)
                                                            em-prefix ("        new-renamed-file\n" 'font-lock-face 'pt:leaf-face)
                                                            em-prefix ("    file-11\n" 'font-lock-face 'pt:leaf-face))
                                        empty-margin
                                        file-12-bs
                                        (make-buffer-string ("dir-2\n" 'font-lock-face 'pt:intr-face)
                                                            ("    dir-21\n" 'font-lock-face 'pt:intr-face)
                                                            em-prefix ("        file-211\n" 'font-lock-face 'pt:leaf-face)
                                                            em-prefix ("    file-22\n" 'font-lock-face 'pt:leaf-face)
                                                            em-prefix ("file-3\n" 'font-lock-face 'pt:leaf-face)
                                                            em-prefix ("file-4\n" 'font-lock-face 'pt:leaf-face))))])

      (send (node-ref 'file-12.rkt) select-as-new!)

      (test-case
       "create-test!"
       (send (current-node) create-test!)

       (check-equal? (current-buffer-content)
                     (make-buffer-content
                      (make-buffer-string ("    dir-1-file-12-test.rkt\n" 'font-lock-face 'pt:leaf-face))
                      (make-buffer-string gray-test-margin
                                          ("    file-12.rkt\n" 'font-lock-face current-leaf-value)))))

      
      (test-case
       "cache-project! with test"
       (send current-project cache-project!)
       (check-equal? current-project
                     (send projects-node read-project "test-dir"))
       
       (send+ (get-field current-project projects-node) (tree-buffer) (clear-buffer!))
       (send projects-node load-project! "test-dir")
       (check-equal? (get-field content (send (get-field current-project projects-node) tree-buffer))
                     (make-buffer-content
                      (make-buffer-string ("    dir-1-file-12-test.rkt\n" 'font-lock-face 'pt:leaf-face))
                      (make-buffer-string gray-test-margin
                                          ("    file-12.rkt\n" 'font-lock-face current-leaf-value)))))
      
    
    (test-case
     "run!"
     (define content-with-running-node
       (make-buffer-content
        (make-buffer-string ("    dir-1-file-12-test.rkt\n" 'font-lock-face 'pt:leaf-face))
        (make-buffer-string gray-test-margin
                            ("    file-12.rkt\n" 'font-lock-face current+exec-leaf-value))))
     (send (current-node) run! 'does-not-matter)
     
     (check-equal? (current-buffer-content)
                   content-with-running-node)

     (send (current-node) on-exit-status! 0)
     (check-equal? (current-buffer-content)
                   (make-buffer-content
                    (make-buffer-string ("    dir-1-file-12-test.rkt\n" 'font-lock-face 'pt:leaf-face))
                    (make-buffer-string gray-test-margin
                                        ("    file-12.rkt\n" 'font-lock-face current-leaf-value))))

     (define current-test (get-field test-module (current-node)))
     (send current-test run! 'does-not-matter)
     (check-equal? (current-buffer-content)
                   (make-buffer-content
                    (make-buffer-string ("    dir-1-file-12-test.rkt\n" 'font-lock-face exec-leaf-value))
                    (make-buffer-string gray-test-margin
                                        ("    file-12.rkt\n" 'font-lock-face current-leaf-value))))
     
     (send* current-test
       (on-test-result! 1)
       (on-exit-status! 0))
     (check-equal? (current-buffer-content)
                   (make-buffer-content
                    (make-buffer-string ("    dir-1-file-12-test.rkt\n" 'font-lock-face 'pt:leaf-face))
                    (make-buffer-string red-test-margin
                                        ("    file-12.rkt\n" 'font-lock-face current-leaf-value))))
     
     (send current-test run! 'does-not-matter)
     (check-equal? (current-buffer-content)
                   (make-buffer-content
                    (make-buffer-string ("    dir-1-file-12-test.rkt\n" 'font-lock-face exec-leaf-value))
                    (make-buffer-string red-test-margin
                                        ("    file-12.rkt\n" 'font-lock-face current-leaf-value))))

     (send* current-test
       (on-test-result! 0)
       (on-exit-status! 0))
     (check-equal? (current-buffer-content)
                   (make-buffer-content
                    (make-buffer-string ("    dir-1-file-12-test.rkt\n" 'font-lock-face 'pt:leaf-face))
                    (make-buffer-string gray-test-margin
                                        ("    file-12.rkt\n" 'font-lock-face current-leaf-value))))

     
     )
    
    )
        
    
   (delete-directory/files "/home/god/test-dir"))
  
  
  )))



