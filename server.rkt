#lang racket/base

(require ss/racket/class

         ss-rpc-server
         
         "constants.rkt"
         
         "tree/file.rkt"
         "tree/project/projects-directory.rkt"
         "tree/ebuffer.rkt"

         "backend/emacs.rkt"

         
         (for-syntax racket/base
                     racket/function
                     
                     ss/racket/syntax
                     
                     syntax/parse
                     syntax/stx))




(define-syntax (produce-epc-methods stx)
  
  (syntax-parse stx
    [(_ #:on-object object:expr
        #:prefix prefix
        #:methods method:id ...)
     (with-syntax ([(method-name ...) (stx-map (curryr prefix-id #'prefix) #'(method ...))])
                   
       #'(begin (define-method (method-name . args)
                  (send object method . args))
                ...))]))




(define projects-node (file:new-descendant-from-path projects-directory% const:projects-path))

(on-terminate (λ () (send projects-node cache-projects!)))

(produce-epc-methods
 #:on-object projects-node
 #:prefix pt:
 #:methods
 new-project-from-existing-dir!
 new-project!
 cache-projects!
 load-project!)

(produce-epc-methods
 #:on-object (get-field current-project projects-node)
 #:prefix pt:
 #:methods
 lift-current-node! lower-current-node!
 ;select-by-name!
 initialize-git-repository!
 switch-to-current-project-node!
 complete-word)

(produce-epc-methods
 #:on-object (get-field+ projects-node current-project git-root)
 #:prefix gt:
 #:methods
 switch-to-git-tree-buffer!
 commit!
 append-branch!
 checkout-master!)


(produce-epc-methods
 #:on-object (get-field+ projects-node current-project git-root current-node)
 #:prefix gt:
 #:methods 
 select-next-leaf! select-prev-leaf!
 select-next-intr! select-prev-intr!
 checkout!
 delete-branch!
 merge!)


(produce-epc-methods
 #:on-object (get-field+ projects-node current-project current-node)
 #:prefix pt:
 #:methods
 select-next-leaf! select-prev-leaf!
 select-next-leaf-4! select-prev-leaf-4!
 select-next-intr! select-prev-intr!
 add-directory! add-file! entered-directory-path
 rename!
 remove-from-tree! delete!
 switch-on-indicator! switch-off-indicator!
 create-test! toggle-test! delete-test!
 run-module-at-foreground! run-test-at-background!
 interrupt-execution!
 test-buffer-name)


(produce-epc-methods
 #:on-object (send (get-field current-project projects-node) current-running-module)
 #:prefix pt:
 #:methods
 on-exit-status! on-unexpected-status! on-test-result!)




;; (produce-epc-methods
;;  #:on-object (get-field current-leaf current-project)
;;  #:prefix ft-
;; ; #:for-side-effects? #t
;;  #:methods lift-if-possible lower-if-possible)



;; (define-epc-method (project-next-leaf)
;;   (send current-project select-next))


;; (new-project-from-dir! "python-epc")
;; (get-field name (send projects-node root))
;; (get-field name (send (car (get-field children (car (get-field children projects-node))))
;;                       project-root))


(parameterize ([emacs genuine-emacs])
  (serve! #:log-level 'debug
          #:log-out (open-output-file "/home/kotik/devtools.log" #:mode 'text #:exists 'replace)))


