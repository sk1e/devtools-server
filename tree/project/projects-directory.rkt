 #lang racket/base

(require racket/contract
         
         ss/racket/class

         "leaf.rkt"
         "test.rkt"
         "module.rkt"
         "root.rkt"
         "intr.rkt"
         "../file.rkt"
         "../ebuffer.rkt"
         "../serialization.rkt"
         "../../constants.rkt"
         "../../backend/emacs.rkt"
         )

(provide projects-directory%)


(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))


(namespace-set-variable-value! 'modified-indicator% ebuffer:modified-indicator% #f ns)
(namespace-set-variable-value! 'test-indicator% ebuffer:test-indicator% #f ns)


(define projects-directory<%>
  (interface (file:intr<%>)
    [init-new-project! (->m string? void?)]
    
    [new-project-from-existing-dir! (->m string? void?)]
    [new-project! (->m void?)]
    
    [read-project (->m string? (is-a?/c root<%>))]
    [load-project! (->m string? void?)]
    [cache-projects! (->m void?)]

    [remove-project! (->m (is-a?/c root<%>) void?)]
    ))


(define projects-directory-mixin
  (mixin (file:intr<%>) (projects-directory<%>)
    (super-new)

    (field [current-project #f])
    
    (inherit new-directory
             absolute-path
             remove-child!)
    
    (inherit-field children)

    (define project-ht #hash())
    
    (define/override (push-child! node)
      (super push-child! node)
      (set! project-ht (hash-set project-ht (path->string (get-field name node)) node)))
    
    (define (project-cache-path name)
      (build-path (absolute-path) name const:project-cache-file-name))
    
    (define (cache-exists? name)
      (file-exists? (project-cache-path name)))

    (define/public (init-new-project! name)
      (set! current-project (new-directory name))
      (push-child! current-project))

    
    (define/public (remove-project! project)
      (remove-child! project)
      (set! project-ht (hash-remove project-ht (path->string (get-field name project)))))
    
    
    (define/public (new-project!)
      (define project-name (send (emacs) direct-call 'read-string "project name: "))
      (cond
       [(directory-exists? (build-path (absolute-path) project-name))
        (error 'bad-name "such project already exists")]
       
       [else
        (define first-file (send (emacs) direct-call 'read-string "first file name: "))
        
        (make-directory (build-path (absolute-path) project-name))
        (call-with-output-file (build-path (absolute-path) project-name first-file) void)
        (new-project-from-existing-dir! project-name)
        
        ;; (init-new-project! project-name)
        ;; (send* current-project
        ;;   (make-directory!)
        ;;   (init-tree-buffer!))
        ]))
    
    (define/public (new-project-from-existing-dir! name)
      (when (or (not (cache-exists? name))
                (equal? 't (send (emacs) direct-call 'yes-or-no-p "there is a cache for this project, make new project anyway? ")))

        (init-new-project! name)
        
        (send* current-project
           (fill-recursively #:filter-not-rx #rx"^backup$|^[.]|^compiled$|^tests$")
          (init-tree-buffer!)
          (init-file-buffers!))


        (define project-children (get-field children current-project))

        (when (pair? project-children)
          (cond
           [(send current-project first-leaf) => (method select!)]
           [else (send (car project-children) select!)]))))
    
    

    (define/public (read-project name)
      (read-node (build-path (absolute-path) name const:project-cache-file-name) ns))
    
    (define/public (load-project! name)
      (cond
       [(hash-has-key? project-ht name)
        (set! current-project (hash-ref project-ht name))
        (send current-project insert-tree!)
        (cond
         [(get-field current-node current-project) => (method switch-to-buffer!)])]
       
       [else
        (set! current-project (read-project name))
        (push-child! current-project)
        
        (send* current-project
          (insert-tree!)
          (init-file-buffers!)
          (init-word-autocomplete!))
        (send (get-field current-node current-project) select!)]))
    
    
    
    (define/public (cache-projects!)
      (for-each (method cache-project!) children))
    
    
    ))



(define projects-directory%
  (class (projects-directory-mixin file:simple-directory%)
    (super-new)
    
    (define/override (child-directory%) root%)
    
    ))



