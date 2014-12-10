#lang racket/base

(require racket/match
         racket/function
         racket/file
         
         ss/racket/class
         rackunit
         rackunit/text-ui
         
         "../tree/file.rkt"
         "../tree/edsl/edsl.rkt"
         "../tree/edsl/utils.rkt")








(define (path-list . v) (map string->path v))

(with-nodes
 #:leaf (file-name-refine-mixin (symboled-node-mixin file:simple-file%))
 #:intr (file-name-refine-mixin (symboled-node-mixin file:simple-directory%))
 #:root (file-name-refine-mixin (symboled-node-mixin file:root%))

 (run-tests
  (test-suite
   "file tree test"

   
   (after
    
    (make-os-file-tree "/home/god/"
                       '((test-dir (dir-1 file-11
                                          file-12)
                                   (dir-2 (dir-21 file-211)
                                          file-22)
                                   (backup bad-file)
                                   (.internal-dir bad-file)
                                   file-3
                                   file-4
                                   .internal-file)))
    
    (define test-dir-object (file:new-descendant-from-path file:simple-directory% "/home/god/test-dir"))
    
    (with-tree #:annotation AU
               #:nodes (/ (home (god (test-dir (dir-1 file-11
                                                      file-12)
                                               (dir-2 (dir-21 file-211)
                                                      file-22)
                                               file-3
                                               file-4))))

               
     
      (test-case
       "descendant from path constructor"
       
       (check-equal? (send test-dir-object root)
                     (make-tree (/ (home (god (test-dir))))))
       
       
       (send test-dir-object fill-recursively #:filter-not-rx #rx"^backup$|^[.]")
       
       (check-equal? (send test-dir-object root) /))


      
      (test-case
       "absolute-path"
       (check-equal? (send test-dir-object absolute-path) (string->path "/home/god/test-dir")))

      (test-case
       "node-path-list"
       (check-equal? (send test-dir-object node-path-list)
                     (path-list "/" "home" "god" "test-dir")))
      
      (test-case
       "ancestor-path?"
       (check-true (send test-dir-object ancestor-path? "/home/god/"))
       (check-true (send test-dir-object ancestor-path? "/home/"))
       (check-true (send test-dir-object ancestor-path? "/")))
      

      (test-case
       "descendant-path?"
       (check-true (send test-dir-object descendant-path? "/home/god/test-dir/qwe"))
       (check-true (send test-dir-object descendant-path? "/home/god/test-dir/qwe/ewq")))


      (test-case
       "find-by-list"

       (check-equal? (send test-dir-object find-by-list (path-list "dir-2" "dir-21")) dir-21))
      
      
      (test-case
       "find-by-path"
       (check-equal? (send test-dir-object find-by-path "/home/god/test-dir/dir-1") dir-1)
       (check-equal? (send test-dir-object find-by-path "/home/god/test-dir/dir-1/file-11") file-11))
      

      )
    
    (delete-directory/files "/home/god/test-dir")))))


