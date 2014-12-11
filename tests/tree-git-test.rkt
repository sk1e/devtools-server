#lang racket/base

(require racket/file
         racket/function
         racket/string
         racket/format
         racket/port
         racket/list
         
         ss/racket/class
         
         rackunit
         rackunit/text-ui
         
         "../tree/project.rkt"
         "../tree/file.rkt"
         "../tree/git-log-grammar.rkt"
         (rename-in "../tree/git.rkt"
                    (git:emulated-root% root%)
                    (git:emulated-intr% intr%)
                    (git:emulated-leaf% leaf%))
         "../backend/specifier.rkt"
         "../backend/buffer.rkt"
         
         "../tree/edsl/edsl.rkt"
         "../tree/edsl/utils.rkt"
         
         "../constants.rkt")



(define test-project-path (build-path const:projects-path "test-project"))

(run-tests
 (test-suite
  "project tree test"

  
  
  (after
   (make-os-file-tree const:projects-path
                      '((test-project (dir-1 file-11
                                             file-12.rkt)
                                      (dir-2 (dir-21 file-211)
                                             file-22)
                                      file-3
                                      file-4)))

   (define (write-to-file project-path string)
     (call-with-output-file (build-path const:projects-path "test-project" project-path)
       (curry write-string string)
       #:exists 'append))

   (define (file-content project-path)
     (call-with-input-file (build-path const:projects-path "test-project" project-path)
       port->string
       #:mode 'text))
   
   (define projects-node (file:new-descendant-from-path project:emulated-projects-directory% const:projects-path))
   

   (send projects-node new-project-from-existing-dir! "test-project")
   (define project-root (get-field current-project projects-node))   
   
   

   
   ;; (define node-ht (send current-project make-ht (compose string->symbol path->string (field-getter name))))
   ;; (define node-ref (curry hash-ref node-ht))
   
   (test-case
    "initialize-repository!"
    (send project-root initialize-git-repository!)
    (check-true (directory-exists? (build-path test-project-path ".git"))))
   
   (define git-root (get-field git-root project-root))
   (define (git-buffer-content) (get-field content (send git-root tree-buffer)))
   (define (git-buffer-header) (get-field header (send git-root tree-buffer)))
   (define (git-commit! message)
     (parameterize ([read-string-param message])
       (send (get-field current-branch git-root) commit!)))

   (test-case
    "git/project tree header"
    (check-equal? (git-buffer-header)
                  (make-buffer-string (" test-project" 'font-lock-face 'sp:root-face)
                                      ("/master" 'font-lock-face 'sp:root-face)))
    
    (check-equal? (get-field header (send project-root tree-buffer))
                  (make-buffer-string (" test-project" 'font-lock-face 'sp:root-face)
                                      ("/master" 'font-lock-face 'sp:root-face))))
   
   (test-case
    "untracked-files"

    (check-equal? (send git-root untracked-files) '("dir-1/" "dir-2/" "file-3" "file-4")))


   (define file-path "dir-1/file-11")
   
   (test-case
    "read/write"
    (write-to-file file-path "mod-1 ")
    (check-equal? (file-content file-path) "mod-1 "))


   
   (test-case
    "commit!"
    (git-commit! "commit-1")
    (check-equal? (git-buffer-content)
                  (make-buffer-string ("initial commit\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-1\n" 'font-lock-face
                                       '(:inherit pt:leaf-face
                                                  :foreground "DeepSkyBlue"
                                                  :background "gray19"))))

    
    (write-to-file file-path "mod-2 ")
    (git-commit! "commit-2")
    (check-equal? (git-buffer-content)
                  (make-buffer-string ("initial commit\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-1\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-2\n" 'font-lock-face
                                       '(:inherit pt:leaf-face
                                                  :foreground "DeepSkyBlue"
                                                  :background "gray19")))))
   
   
   (test-case
    "commit checkout!"
    (send (second (get-field children git-root)) checkout!)
    (check-equal? (git-buffer-content)
                  (make-buffer-string ("initial commit\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-1\n" 'font-lock-face
                                       '(:inherit pt:leaf-face
                                                  :foreground "DeepSkyBlue"))
                                      ("commit-2\n" 'font-lock-face
                                       '(:inherit pt:leaf-face
                                                  :background "gray19"))))
    (check-equal? (file-content file-path) "mod-1 ")

    (define commit-2 (third (get-field children git-root)))
    (send commit-2 checkout!)
    (check-equal? (file-content file-path) "mod-1 mod-2 ")
    
    (check-equal? (git-buffer-header)
                  (make-buffer-string ((format "~a ~a"
                                               (get-field checksum commit-2)
                                               (get-field message commit-2))
                                       'font-lock-face 'gt:root-detached-face))))

   ;(define )
   (define start-tag-accessor (compose meta-start-tag commit-meta))
   (define end-tag-accessor (compose meta-end-tag commit-meta))

   (define (nth-commit n)
     (list-ref (reverse (send git-root parsed-git-log)) n))
   
   (define (get-tag n accessor)
     (accessor (nth-commit n)))

   (test-case
    "add-branch!"
    (send git-root add-branch! "branch")
    
    (check-equal? (git-buffer-content)
                  (make-buffer-string ("initial commit\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-1\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-2\n" 'font-lock-face
                                       '(:inherit pt:leaf-face :foreground "DeepSkyBlue"))
                                      ("branch\n" 'font-lock-face
                                       '(:inherit pt:intr-face :background "gray19"))))
    
    (check-equal? (git-buffer-header)
                  (make-buffer-string (" test-project" 'font-lock-face 'sp:root-face)
                                      ("/master" 'font-lock-face 'sp:root-face)
                                      ("/branch" 'font-lock-face 'gt:root-unmerged-face))))
    

   
   
    ;; (check-equal? (get-tag 2 start-tag-accessor) "branch"))
   
   (define branch (list-ref (get-field children git-root) 3))
   
   
   (test-case
    "commit! to branch"
    (write-to-file file-path "mod-3 ")
    (git-commit! "commit-3")
    
    (check-equal? (git-buffer-content)
                  (make-buffer-string ("initial commit\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-1\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-2\n" 'font-lock-face 'pt:leaf-face)
                                      ("branch\n" 'font-lock-face 'pt:intr-face)
                                      ("    commit-3\n" 'font-lock-face
                                       '(:inherit pt:leaf-face :foreground "DeepSkyBlue" :background "gray19"))))

    (check-equal? (get-tag 3 end-tag-accessor) 'unmerged)
    (write-to-file file-path "mod-4 ")
    (git-commit! "commit-4")

    ;; (displayln (send git-root parsed-git-log))
    ;; (displayln (send git-root project-subprocess/string-output "git log --oneline --decorate --branches"))
    ;; git log order fails here sometimes
    ;; (check-false (commit-meta (nth-commit 3)))
    )


   (test-case
    "branch checkout!"
    (send git-root checkout!)
    (check-equal? (git-buffer-content)
                  (make-buffer-string ("initial commit\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-1\n" 'font-lock-face 'pt:leaf-face)
                                      ("commit-2\n" 'font-lock-face
                                       '(:inherit pt:leaf-face :foreground "DeepSkyBlue" :background "gray19"))
                                      ("branch\n" 'font-lock-face 'pt:intr-face)
                                      ("    commit-3\n" 'font-lock-face 'pt:leaf-face)
                                      ("    commit-4\n" 'font-lock-face 'pt:leaf-face)))


    (check-equal? (file-content file-path) "mod-1 mod-2 ")
    (check-equal? (git-buffer-header)
                  (make-buffer-string (" test-project" 'font-lock-face 'sp:root-face)
                                      ("/master" 'font-lock-face 'sp:root-face))))

   
   (test-case
    "branch merge!"
    (send branch merge!)
    
    (check-equal? (get-field current-branch git-root) git-root)
    (check-equal? (file-content file-path) "mod-1 mod-2 mod-3 mod-4 ")
    
    (check-equal? (get-field current-version git-root)
                  (second (get-field children branch)))
    
    (check-equal? (git-buffer-header)
                  (make-buffer-string (" test-project" 'font-lock-face 'sp:root-face)
                                      ("/master" 'font-lock-face 'sp:root-face)))

    (send branch checkout!)

    (check-equal? (git-buffer-header)
                  (make-buffer-string (" test-project" 'font-lock-face 'sp:root-face)
                                      ("/master" 'font-lock-face 'sp:root-face)
                                      ("/branch" 'font-lock-face 'sp:root-face)))

    (write-to-file file-path "bad-mod-4 ")
    (check-exn exn:fail? (λ () (git-commit! "bad commit-4"))))

   
   ;; (test-case
   ;;  "delete-branch!"
   ;;  (send git-root add-branch! "new-branch")
   ;;  (define new-branch (get-field current-branch git-root))
   ;;  (write-to-file file-path "bad-mod-5 ")
   ;;  (git-commit! "bad commit-5")
   ;;  (send new-branch delete-branch!)
    
   ;;  (check-equal? (git-buffer-content)
   ;;                (make-buffer-string ("initial commit\n" 'font-lock-face 'pt:leaf-face)
   ;;                                    ("commit-1\n" 'font-lock-face 'pt:leaf-face)
   ;;                                    ("commit-2\n" 'font-lock-face 'pt:leaf-face)
   ;;                                    ("branch\n" 'font-lock-face 'pt:intr-face)
   ;;                                    ("    commit-3\n" 'font-lock-face
   ;;                                     '(:inherit pt:leaf-face :foreground "DeepSkyBlue" :background "gray19"))))

   ;;  (check-equal? (file-content file-path) "mod-1 mod-2 mod-3 "))
   
   
   (define (make-git-log meta+message-part-list)
     (map (λ (part idx) (format "~a ~a"
                                (~r idx #:min-width 7 #:pad-string "0")
                                part))
          meta+message-part-list
          (reverse (range (length meta+message-part-list)))))
   

   ;; (define (root-from-log . commits)
   ;;   (define root (make root%))
   ;;   (send root infer! (make-git-log commits) '() root)
   ;;   root)

   
   (define (root-from-log #:limit [limit 100] . commits)
     (define root (make root%))
     ;; (displayln (apply string-append
     ;;                   (make-git-log commits)))
     (send root
           infer!
           (parse-log (open-input-string (string-join (make-git-log commits) "\n" #:after-last "\n")))
           '()
           root
           limit)
     root)

   
   
   
   (test-equal? "infer! simple"
                (root-from-log "(HEAD, master) cc-2"
                                 "cc-1"
                                 "cc-0")
                (make root% [children (list (make leaf% [checksum "0000000"] [message "cc-0"])
                                            (make leaf% [checksum "0000001"] [message "cc-1"])
                                            (make leaf% [checksum "0000002"] [message "cc-2"]))]))
   

   
   ;; cc-0
   ;; cc-1
   ;; bb
   ;;   cc-2
   ;;   cc-3
   ;; cc-4
   ;; cc-5
   
   (test-equal? "infer! branch"
                (root-from-log "(HEAD, master) cc-5"
                               "cc-4"
                               "(tag: bb-merged-branch-end) cc-3"
                               "cc-2"
                               "(tag: bb-branch-start) cc-1"
                               "cc-0")
                (make root% [children (list (make leaf% [checksum "0000000"] [message "cc-0"])
                                            (make leaf% [checksum "0000001"] [message "cc-1"])
                                            (make intr%
                                              [name "bb"]
                                              [merged? #t]
                                              [children (list (make leaf% [checksum "0000002"] [message "cc-2"])
                                                              (make leaf% [checksum "0000003"] [message "cc-3"]))])
                                            (make leaf% [checksum "0000004"] [message "cc-4"])
                                            (make leaf% [checksum "0000005"] [message "cc-5"]))]))
   
   ;; cc-0
   ;; bb-1
   ;;   cc-1
   ;;   bb-2
   ;;     cc-2
   ;;   cc-3
   
   
   (test-equal? "infer! nested"
                (root-from-log "(HEAD, tag: bb-1-unmerged-branch-end, master) cc-3"
                               "(tag: bb-2-merged-branch-end) cc-2"
                               "(tag: bb-2-branch-start) cc-1"
                               "(tag: bb-1-branch-start) cc-0")
                (make root% [children (list (make leaf% [checksum "0000000"] [message "cc-0"])
                                            (make intr%
                                              [name "bb-1"]
                                              [children (list (make leaf% [checksum "0000001"] [message "cc-1"])
                                                              (make intr%
                                                                [name "bb-2"]
                                                                [merged? #t]
                                                                [children (list (make leaf% [checksum "0000002"] [message "cc-2"]))])
                                                              (make leaf% [checksum "0000003"] [message "cc-3"]))]))]))
   
   
   


   (test-case
    "infer! limit"
    
    ;; cc-0
    ;; cc-1
    ;; bb-1
    ;;   cc-2
    ;;   bb-2
    ;;     cc-3
    ;;   cc-4
    ;; cc-5
    
    (define bb-1 (make intr%
                   [name "bb-1"]
                   [merged? #t]
                   [children (list (make leaf% [checksum "0000002"] [message "cc-2"])
                                   (make intr%
                                     [name "bb-2"]
                                     [merged? #t]
                                     [children (list (make leaf% [checksum "0000003"] [message "cc-3"]))])
                                   (make leaf% [checksum "0000004"] [message "cc-4"]))]))

    (define (limited-log n)
      (apply root-from-log
             '("(HEAD) cc-5"
               "(tag: bb-1-merged-branch-end, master) cc-4"
               "(tag: bb-2-merged-branch-end) cc-3"
               "(tag: bb-2-branch-start) cc-2"
               "(tag: bb-1-branch-start) cc-1"
               "cc-0")
             #:limit n))

    (check-equal? (limited-log 1)
                  (make root%
                    [children (list (make leaf% [checksum "0000005"] [message "cc-5"]))]))

    (check-equal? (limited-log 2)
                  (make root%
                    [children (list bb-1
                                    (make leaf% [checksum "0000005"] [message "cc-5"]))]))

    (check-equal? (limited-log 4)
                  (make root%
                    [children (list bb-1
                                    (make leaf% [checksum "0000005"] [message "cc-5"]))]))

    (check-equal? (limited-log 5)
                  (make root%
                    [children (list (make leaf% [checksum "0000001"] [message "cc-1"])
                                    bb-1
                                    (make leaf% [checksum "0000005"] [message "cc-5"]))])))
   
   (delete-directory/files test-project-path))))



