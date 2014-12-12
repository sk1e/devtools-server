#lang racket/base

(require racket/function
         racket/contract
         racket/match
         racket/string
         racket/list
         
         ss/racket/class
         ss/racket/provide

         "base.rkt"
         "file.rkt"
         "ebuffer.rkt"
         "serialization.rkt"
         "../constants.rkt"
         "../backend/emacs.rkt"
         "../backend/buffer.rkt")


(provide (prefix-out project: (suffixed-as interface class
                                           #:from (all-defined-out))))



(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(namespace-set-variable-value! 'modified-indicator% ebuffer:modified-indicator% #f ns)
(namespace-set-variable-value! 'test-indicator% ebuffer:test-indicator% #f ns)
;(namespace-set-variable-value! 'empty-indicator% ebuffer:empty-indicator%% #f ns)


(define (leaf? v)       (is-a? v leaf<%>))
(define (intr? v)       (is-a? v intr<%>))
(define (descendant? v) (is-a? v descendant<%>))
(define (runnable-leaf? v) (is-a? v runnable-leaf<%>))


(define-logger project)
;; log-project-debug


(define node<%>
  (interface (file:node<%> ebuffer:node<%>)
    [project-path (->m path-string?)]
    ))

(define node-mixin
  (mixin (file:node<%> ebuffer:node<%>) (node<%>)
    (super-new)
    
    (inherit-field name)


    (abstract project-path)

    (define/override (get-name) (path->string name))
    
    (define/override (tree-buffer) (get-field project-buffer (emacs)))



    ))

(define descendant<%>
  (interface (node<%> ebuffer:descendant<%>)
    [entered-directory (-> ebuffer:current-node? intr?)]
    [entered-directory-path (-> ebuffer:current-node? path-string?)]
    
    [add-directory! (-> ebuffer:current-node? string? void?)]
    [add-file!      (-> ebuffer:current-node? string? void?)]
    
    [initialize-project-node! (->m void?)]
    [delete! (->m void?)]
    [rename! (->m void?)]
    [pre-rename! (->m string? void?)]

    ))


(define descendant-mixin
  (mixin (node<%> ebuffer:descendant<%> file:descendant<%>) (descendant<%>)
    (super-new)

    (inherit-field parent name)
    
    (inherit select-as-new!
             clear-solo!
             insert/shift-solo!
             mark-as-selected!
             get-name
             init-name!
             rename-file-or-directory!
             remove-file-or-directory!
             absolute-path
             last-descendant-or-self
             prev-leaf)
    
    
    (abstract entered-directory
              initialize-project-node!
              pre-rename!)

    (define/override (remove-from-tree!)
      (cond
       [(or (prev-leaf) (send (last-descendant-or-self) next-leaf)) => (method select!)])
      (super remove-from-tree!))

    
    (define/public (entered-directory-path) (path->string (send (entered-directory) absolute-path)))
    
    (define/public (add-directory! new-name)
      (define new-dir (send (entered-directory) new-directory new-name))
      (send (entered-directory) add-project-node/select! new-dir)
      (send new-dir make-directory-if-not!))

    (define/public (add-file! new-name)
      (define new-file (send (entered-directory) new-file new-name))
      (send (entered-directory) add-project-node/select! new-file)
      (send new-file make-file-if-not!))
    
    
    (define/public (delete!)
      (when (equal? 't (send (emacs) direct-call 'yes-or-no-p (format "delete ~a?" (path->string (absolute-path)))))
        (remove-from-tree!)
        (remove-file-or-directory!)))
    
    (define/public (rename!)
      (rename-desc! (send (emacs) direct-call 'read-string "rename to: " (get-name))))


    (define/public (rename-desc! new-name)
      (pre-rename! (path->string (build-path (send parent project-path)
                                             new-name)))
      (rename-file-or-directory! new-name)
      (clear-solo!)
      (init-name! new-name)
      (insert/shift-solo!))


    (define/override (project-path)
      (build-path (send parent project-path) name))
    
    
    
    ))


(define ancestor<%>
  (interface (node<%>)
    [add-project-node! (->m descendant? void?)]))


(define ancestor-mixin
  (mixin (node<%> ebuffer:ancestor<%> file:ancestor<%>) (ancestor<%>)
    (super-new)

    (inherit-field children)

    (inherit push-child!)

    (define/override (child-directory%) directory%)

    (define/override (child-file% name)
      (match name
        [(regexp #rx"[.]rkt$") module%]
        [_ file%]))
    
    (define/public (add-project-node! node)
      (push-child! node)
      (send* node
        (initialize-ebuffer-node!)
        (initialize-project-node!)))


    (define/public (add-project-node/select! node)
      (add-project-node! node)
      (send node select-as-new!))

    ))



(define (test-dir-implementation? %) (implementation? % test-directory<%>))

(define root<%>
  (interface (base:quasiroot<%> file:intr<%> ancestor<%>)
    [init-file-buffers! (->m void?)]
    [cache-project! (->m void?)]
    [init-test-directory-if-not! (->m void?)]
    
    [push-running-module! (->m (cons/c runnable-leaf? symbol?) void?)]
    [drop-running-module! (->m void?)]
    [current-running-module (->m runnable-leaf?)]
    [run-other-if-left! (->m void?)]

    ;; [initialize-git-repository! (->m void?)]
    [get-name-header (->m buffer-string?)]
    [switch-to-current-project-node! (->m void?)]
    ;; [reload-project! (->m void?)]
    ))



(define root-mixin
  (mixin (base:quasiroot<%> file:intr<%> ebuffer:root<%> ancestor<%>) (root<%>)
    (super-new)

    (field [test-directory #f]
           ;; [git-root #f]
           )
    
    (inherit-field current-node name parent)
    
    (inherit leafs
             tree-buffer
             get-name
             cache-node!
             absolute-path
             node-identifier
             new-directory
             add-project-node!)

    
    (define/override (field-setter-exprs)
      (cond
       [test-directory (cons `(set-field! test-directory ,(node-identifier) ,(send test-directory node-identifier))
                             (super field-setter-exprs))]
       [else (super field-setter-exprs)]))

    

    (define/override (next-neighbour) #f)
    
    (define/override (init-insert!)
      (send (tree-buffer) call
            `(lambda () (setq header-line-format (propertize ,(string-append " " (get-name))
                                                             'font-lock-face
                                                             '(:weight ultra-bold
                                                                       :height 110
                                                                       :family "Liberation Mono"
                                                                       :foreground "CornflowerBlue"))))))

    ;; (define/override (pre-tree-insert!)
    ;;   (super pre-tree-insert!)      
    ;;   (send (tree-buffer) set-header! (cond
    ;;                                    [git-root (send (get-name-header) concat (send git-root header-suffix))]
    ;;                                    [else (get-name-header)])))


    (define/public (switch-to-current-project-node!)
      (send (get-field buffer current-node) switch-to-buffer!))
    
    (define/public (get-name-header)
      (make-buffer-string ((string-append " " (get-name))
                           'font-lock-face 'sp:root-face)))
    


    ;; (define/public (initialize-git-repository!)
    ;;   (set! git-root (new (git-root%)))
    ;;   (send git-root initialize-repository! this))
    
    
    

    
    (define/public (init-file-buffers!)
      (define leaf-nodes (leafs))
      (for-each (method initialize-project-node!) leaf-nodes)
      (send (car leaf-nodes) switch-to-buffer!))



    (define/public (cache-project!)
      (cache-node! (build-path (absolute-path) const:project-cache-file-name)))

    ;; (define/public (reload-project!)
    ;;   (for-each (compose (method kill!) (method leaf-buffer))
    ;;             (leafs))
    ;;   (send* parent
    ;;     (remove-project! this)
    ;;     (load-project! (path->string name))))

    (define/override (project-path) (string->path "/"))

    (define/public (init-test-directory-if-not!)
      (unless test-directory
        (set! test-directory (make test-directory% [name "tests"]))
        (add-project-node! test-directory)
        (send test-directory make-directory-if-not!)))


    
    (define running-modules '())

    (define/public (push-running-module! v)
      (set! running-modules (cons v running-modules)))

    (define/public (drop-running-module!)
      (set! running-modules (cdr running-modules)))

    (define/public (running-module? v)
      (member v running-modules (λ (v e) (eq? v (car e)))))
    

    (define/public (current-running-module)
      (caar running-modules))

    (define/public (run-other-if-left!)
      (match running-modules
        ['() (void)]
        [(cons (cons module executor) _) (send module run-start! executor)]))

    
    
    ))



(define intr<%>
  (interface (file:intr<%> ebuffer:intr<%> descendant<%> ancestor<%>)
    ))


(define intr-mixin
  (mixin (file:intr<%> descendant<%> ancestor<%>) (intr<%>)
    (super-new)

    (inherit-field children)

    (define/override (pre-rename! new-name)
      (for-each (λ (node)
                   (send node
                         pre-rename!
                         (path->string (build-path new-name
                                                   (get-field name node)))))
                children))
    
    (define/override (post-select!) (void))
    (define/override (initialize-project-node!) (void))
    ;; (define/override (face) 'pt:intr-face)
    (define/override (entered-directory) this)
    
    ))




(define leaf<%>
  (interface (file:leaf<%> descendant<%>)
    [buffer-name (->m string?)]
    [switch-to-buffer! (->m void?)]
    [modified-indicator (->m ebuffer:indicator?)]
    [test-indicator (->m ebuffer:indicator?)]
    [revert-buffer! (->m void?)]
    ))




(define leaf-mixin
  (mixin (file:leaf<%> descendant<%>) (leaf<%>)
    (super-new)

    (inherit absolute-path
             project-path
             root)
    
    (inherit-field parent
                   indicators)


    ;; todo make private
    (field [buffer 'uninitialized])

    
    ;; (define/override (face) 'pt:leaf-face)
    
    (define/override (indicator-list)
      (list (make ebuffer:modified-indicator%
              [position 0]
              [color const:indicator-modified-warning-color]
              [node this])
            (make ebuffer:test-indicator%
              [position 1]
              [color const:indicator-test-base-color]
              [node this])))
    

    (define/public (modified-indicator) (first indicators))
    (define/public (test-indicator) (second indicators))
    
    (define/override (post-select!)
      (switch-to-buffer!))

    (define/override (entered-directory) parent)


    (define/public (switch-to-buffer!)
      (send (emacs) deferred-call 'switch-to-buffer (get-field name buffer)))

    (define/public (revert-buffer!)
      (send buffer revert!))
    
    (define/public (buffer-name)
      (path->string (project-path)))

    (define (buffer-name-font-parts path)
      (define-values (base name _) (split-path path))
      (list (list (path->string base)
                  const:buffer-base-path-face)
            (list (path->string name)
                  const:buffer-name-path-face)))
    
    (define/override (initialize-project-node!)
      (define name (buffer-name))
      (set! buffer (new (send (emacs) buffer%) [name name]))
      (send (emacs) deferred-call 'pt:init-file-buffer
                     (path->string (absolute-path))
                     name
                     (buffer-name-font-parts name)))

    
    (define/override (pre-rename! new-name)
      (set! buffer (new (send (emacs) buffer%) [name new-name]))
      (send (emacs) deferred-call 'pt:rename-file-buffer
                     (buffer-name)
                     new-name
                     (buffer-name-font-parts new-name)))
    
    
    
    ))



(define runnable-leaf<%>
  (interface (leaf<%>)
    [run! (->m symbol? void?)]
    [run-start! (->m symbol? void?)]
    [interrupt-execution! (->m void?)]
    [mark-as-running! (->m void?)]
    [mark-as-not-running! (->m void?)]

    [on-exit-status! (->m integer? void?)]
    [on-normal-exit-status! (->m void?)]
    [on-error-exit-status! (->m void?)]

    [run-module-at-foreground! (->m void?)]
    ))

    
    

(define runnable-leaf-mixin
  (mixin (leaf<%>) (runnable-leaf<%>)
    (super-new)
    
    (inherit-field name)
    
    (inherit absolute-path
             buffer-name
             root
             put-extra-ftf-prop!
             remove-extra-ftf-prop!)

    (abstract run-module-at-foreground!)

    
    (define/public (run-start! elisp-executor)
      (define exec-buffer-name (format "*exec ~a*" (buffer-name)))
      (send (emacs) deferred-call 'pt:init-exec-buffer exec-buffer-name)
      (send (emacs) deferred-call elisp-executor exec-buffer-name (path->string (absolute-path)))
      (mark-as-running!))

    
    (define/public (run! elisp-executor)
      (unless (send (root) running-module? this)
        (send* (root)
          (push-running-module! (cons this elisp-executor))
          (run-other-if-left!))))


    (define/public (interrupt-execution!)
      (send (emacs) deferred-call 'pt:interrupt-process))
    
        
    (define/public (mark-as-running!)
      (put-extra-ftf-prop! ':foreground const:execution-foreground-color))
    
    (define/public (mark-as-not-running!)
      (remove-extra-ftf-prop! ':foreground))
    
    (define/public (on-exit-status! code)
      (mark-as-not-running!)
      (match code
        [0 (on-normal-exit-status!)]
        [1 (on-error-exit-status!)]
        [other (error 'unexpected-exit-status "~a" other)])
      (send* (root) (drop-running-module!) (run-other-if-left!)))

    (define/public (on-normal-exit-status!) (void))
    (define/public (on-error-exit-status!) (void))

    (define/public (on-unexpected-status! status)
      (error 'unexpected-process-status "~a" status))



    
    ))



(define test-leaf<%>
  (interface (runnable-leaf<%>)
    [on-test-result! (->m natural-number/c void?)]
    [toggle-test! (->m void?)]))

(define test-leaf-mixin
  (mixin (leaf<%> runnable-leaf<%>) (test-leaf<%>)
    (super-new)
    (inherit-field name)
    
    (field [tested-module 'uninitialized])

    (inherit node-identifier
             absolute-path
             run!)
    

    (define (deinit-test)
      (set-field! test-module tested-module #f)
      (send+ tested-module (test-indicator) (switch-off!)))
    
    (define/override (remove-from-tree!)
      (super remove-from-tree!)
      (deinit-test))
    
    (define/override (delete!)
      (super delete!)
      (deinit-test))

    
    (define/override (on-error-exit-status!)
      (send+ tested-module
             (test-indicator)
             (change-color! const:indicator-test-warning-color)))

    (define/override (run-module-at-foreground!)
      (run! 'pt:execute-test-at-foreground))

    
    (define/public (on-test-result! failures-num)
      (send+ tested-module
             (test-indicator)
             (change-color! (match failures-num
                              [0 const:indicator-test-base-color]
                              [(? (curryr > 0) _) const:indicator-test-warning-color]
                              [_ (error 'unexpected-failures-number "~a" failures-num)]))))

    (define/public (toggle-test!)
      (send tested-module select-as-new!))    
    
    ))

(define module-leaf<%>
  (interface (runnable-leaf<%>)
    [init-test-module (->m (is-a?/c file:leaf<%>) void?)]
    [test-name (->m string?)]
    [create-test! (->m void?)]
    [toggle-test! (->m void?)]

    [run-test-at-background! (->m void?)]
    ))



(define module-leaf-mixin
  (mixin (leaf<%> runnable-leaf<%>) (module-leaf<%>)
    (field [test-module #f])
    
    (super-new)

    (inherit-field indicators)
    
    (inherit project-path
             root
             buffer-name
             node-identifier
             switch-to-buffer!
             test-indicator
             run!)
    
    (define/override (field-setter-exprs)
      (cond
       [test-module (cons `(send ,(node-identifier) init-test-module ,(send test-module node-identifier))
                          (super field-setter-exprs))]
       [else (super field-setter-exprs)]))
        

        
    (define/public (init-test-module v)
      (set! test-module v)
      (set-field! tested-module test-module this))
    
    (define/public (test-name)
      (string-append (string-join (match (map path->string (cdr (file:path->list (project-path))))
                                    [(list x ... last)
                                     (append x (list (substring last
                                                                0
                                                                (- (string-length last) 4))))])
                                  "-")
                     "-test.rkt"))


    (define/public (test-buffer-name)
      (cond
       [test-module (send test-module buffer-name)]
       [else ""]))
    
    (define/public (create-test!)
      (send (root) init-test-directory-if-not!)
      
      (define test-dir (get-field test-directory (root)))
      (init-test-module (make module-test% [name (test-name)]))
      
      (send test-dir add-project-node! test-module)
      (send (test-indicator) switch-on!)

      (send test-module make-file-if-not!))

    

    (define/public (toggle-test!)
      (when test-module
        (send test-module select-as-new!)))

    
    (define/public (run-test-at-background!)
      (send test-module run! 'pt:execute-test-at-background))

    (define/override (run-module-at-foreground!)
      (run! 'pt:execute-module-at-foreground))


    ))



(define-composed-mixins
  [descendant-sum (node descendant)]
 
  [root-sum       (node ancestor root)]
  [intr-sum       (descendant-sum ancestor intr)]
  [leaf-sum       (descendant-sum leaf)])


(define-composed-mixins  
  [leaf-final-sum (ebuffer:leaf-final-sum      file:leaf-sum leaf-sum)]
  [module-leaf-final-sum (leaf-final-sum       module-leaf)]
  [intr-final-sum (ebuffer:intr-final-sum      file:intr-sum intr-sum)]
  [root-final-sum (ebuffer:quasiroot-final-sum file:intr-sum root-sum)])


(define-inspected-class file%        (class-from-mixins leaf-final-sum))
(define-inspected-class module%      (class-from-mixins leaf-final-sum runnable-leaf module-leaf))
(define-inspected-class module-test% (class-from-mixins leaf-final-sum runnable-leaf test-leaf))
(define-inspected-class directory%   (class-from-mixins intr-final-sum))
(define-inspected-class root%        (class-from-mixins root-final-sum))


(define test-directory<%>
  (interface (file:intr<%>)
    ))



(define test-directory-mixin
  (mixin (intr<%>) (test-directory<%>)
    (super-new)

    (inherit-field parent)

    (inherit node-identifier)


    (define/override (field-setter-exprs)
      (cons `(set-field! parent ,(node-identifier) ,(send parent node-identifier))
            (super field-setter-exprs)))
    
    ))

(define-inspected-class test-directory% (class-from-mixins intr-final-sum test-directory))


(define projects-directory<%>
  (interface (file:intr<%>)
    [init-new-project! (->m string? void?)]
    
    [new-project-from-existing-dir! (->m string? void?)]
    [new-project! (->m string? void?)]
    
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
    
    
    (define/public (new-project! name)
      (init-new-project! name)
      (send* current-project
        (make-directory!)
        (init-tree-buffer)))
    
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
          (init-file-buffers!))
        (send (get-field current-node current-project) mark-as-selected!)]))
    
    
    
    (define/public (cache-projects!)
      (for-each (method cache-project!) children))
    
    
    ))



(define projects-directory%
  (class (projects-directory-mixin file:simple-directory%)
    (super-new)
    
    (define/override (child-directory%) root%)
    
    ))

