#lang racket/base

(require ss/racket/class
         ss/racket/provide

         racket/match
         racket/list
         racket/function
         racket/string
         racket/contract
         racket/port
         racket/system

         srfi/19
         
         "git-log-grammar.rkt"
         "ebuffer.rkt"
         "../constants.rkt"
         "../backend/specifier.rkt"
         "../backend/buffer.rkt")


(provide (prefix-out git: (suffixed-as interface class
                                       #:from (all-defined-out)))
         git-logger)


(define-logger git)

(define (node? v)       (is-a? v node<%>))
(define (descendant? v) (is-a? v descendant<%>))
(define (ancestor? v)   (is-a? v ancestor<%>))
(define (root? v)       (is-a? v root<%>))
(define (intr? v)       (is-a? v intr<%>))
(define (leaf? v)       (is-a? v leaf<%>))

(define node<%>
  (interface (ebuffer:node<%> backend-specifier<%>)
    [project-subprocess/string-output (->m string? string?)]
    [project-subprocess/void-output (->m string? void?)]
    [checkout! (->m void?)]
    [checkout-arg (->m string?)]))


(define node-mixin
  (mixin (ebuffer:node<%> backend-specifier<%>) (node<%>)
    (super-new)

    (inherit get-backend root)

    (abstract project-subprocess/string-output
              project-subprocess/void-output
              checkout-arg)
    
    (define/override (tree-buffer) (get-backend const:git-tree-buffer))

    (define/public (checkout!)
      (project-subprocess/void-output (format "git checkout ~a" (checkout-arg)))
      
      (send (get-field project-tree-root (root)) reload-project!)
      
      (define checkout-project (get-field+ (root) project-tree-root parent current-project))
      (set-field! git-root
                  checkout-project
                  (root))

      (set-field! project-tree-root
                  (root)
                  checkout-project))
    
    ))


(define descendant<%>
  (interface (node<%> ebuffer:descendant<%>)))

(define descendant-mixin
  (mixin (node<%> ebuffer:descendant<%>) (descendant<%>)
    (super-new)

    (inherit root)

    (define/override (project-subprocess/string-output command)
      (send (root) project-subprocess/string-output command))

    (define/override (project-subprocess/void-output command)
      (send (root) project-subprocess/void-output command))

    
    ))



(define ancestor<%>
  (interface (node<%> ebuffer:ancestor<%>)
    [init-name! (->m string? void?)]
    [add-branch! (->m string? void?)]
    [commit! (->m void?)]
    [header-suffix (->m buffer-string?)]
    [commitable? (->m boolean?)]
    [untracked-files (->m (listof string?))]
    [add-files! (->m (listof string?) void?)]
    [branch-head (->m leaf?)]
    ))

(define ancestor-mixin
  (mixin (node<%> ebuffer:ancestor<%>) (ancestor<%>)
    (super-new)
    
    (inherit-field children)
    
    (field [name 'uninitialized])

    (inherit project-subprocess/string-output
             project-subprocess/void-output
             root
             append-child!
             get-backend
             node-identifier
             direct-call)

    (abstract header-suffix
              commitable?)

    (define/override (repr-fields)
      (cons 'name (super repr-fields)))

    (define/override (equality-fields)
      (cons 'name (super equality-fields)))

    (define/override (field-setter-exprs)
      (cons `(set-field! name ,(node-identifier) ,name)
            (super field-setter-exprs)))
    
    (define/override (get-name) name)
    (define/override (checkout-arg) name)

        
    (define/override (checkout!)
      (super checkout!)
      (send* (root)
        (update-current-version!)
        (set-current-branch! this)))
    
    
    
    (define/public (init-name! v) (set! name v))

    (define/public (branch-head)
      ;(and (pair? children) (last children))
      (findf leaf? (reverse children))
      )
    
    
    (define (intr%) (get-backend intr-container))
    (define (leaf%) (get-backend leaf-container))


    (define/public (add-branch! name)      
      (project-subprocess/void-output (format "git checkout -b '~a'" name))
      (define branch (make (intr%) [name name]))
      (append-child! branch)
      (send (root) set-current-branch! branch)
      (send (branch-head) branch-tag branch "branch-start")
      
      (send* branch
        (initialize-ebuffer-node!)
        (select-as-new!)))

    (define/public (git-status) (project-subprocess/string-output "git status -s"))
    
    (define/public (untracked-files)
      (let parse ([status-lines (string-split (git-status) "\n")])
        (match status-lines
          [(cons x xs) (match (string-split x " ")
                         [(list "??" file-name) (cons file-name (parse xs))]
                         [_ (parse xs)])]
          ['() '()])))

    (define/public (add-files! file-list)
      (project-subprocess/void-output (string-append "git add " (string-join file-list))))
    


    (define/contract (group-words word-list max-len)
      (-> (listof string?) natural-number/c (listof (listof string?)))
      (let make ([lst word-list]
                 [words-len 0]
                 [word-acc '()])
        (match lst
          [(cons x xs)
           (define len (string-length x))
           (cond
            [(< (+ words-len len (sub1 (length word-acc)))
                max-len) 
             (make xs (+ words-len len) (cons x word-acc))]
            
            [else (cons (reverse word-acc) (make xs len (list x)))])]
          ['() (list word-acc)])))
    
    (define (make-commit!)
      (define message (direct-call 'read-string "commit message: "))
      (match-define (list _ branch checksum) (regexp-match #px"^\\[(\\S+) (\\S+)\\]"
                                                           (project-subprocess/string-output
                                                            (format "git commit -am '~a'" message))))
      
      (unless (equal? name branch)
        (error 'unsynchronized-current-branch
               "from git output: ~a\nfrom tree: ~a"
               branch
               name))
      
      (define commit (make (leaf%)
                       [message message]
                       [checksum checksum]))

      
      (append-child! commit)
      (send* commit
        (initialize-ebuffer-node!)
        (select-as-new!)
        (select-version-as-new!)))
    
    
    
    (define/public (commit!)
      (unless (commitable?)
        (error 'not-allowed-to-commit-to-merged-branch "~a" this))

      (send (get-field project-tree-root (root)) cache-project!)
       
      (match (untracked-files)
        [(list files ..1) (when (eq? (direct-call 'yes-or-no-p
                                                  (string-append "include untracked files:\n"
                                                                 (string-join (map string-join
                                                                                   (group-words files 80)))
                                                                 "\n? "))
                                     't)
                            (add-files! files)
                            (make-commit!))]
        ['() (make-commit!)]))
      
    ))



(define leaf<%>
  (interface (descendant<%> ebuffer:leaf<%>)
    [init-message! (->m string? void?)]
    [init-checksum! (->m string? void?)]
    [mark-as-cv! (->m void?)]
    [mark-as-not-cv! (->m void?)]
    [select-version! (->m void?)]
    [select-version-as-new! (->m void?)]
    ))


(define leaf-mixin
  (mixin (descendant<%> ebuffer:leaf<%>) (leaf<%>)
    (super-new)
    
    (field [message 'uninitialized]
           [checksum 'uninitialized])
    
    (inherit select-as-new!
             put-extra-ftf-prop!
             remove-extra-ftf-prop!
             root
             node-identifier
             project-subprocess/void-output)

    
    (define/override (repr-fields)
      (append '(checksum message) (super repr-fields)))
    
    (define/override (equality-fields)
      (append '(checksum message) (super equality-fields)))



    (define/override (field-setter-exprs)
      (cons `(set-field! checksum ,(node-identifier) ,checksum)
            (cons `(set-field! message ,(node-identifier) ,message)
                  (super field-setter-exprs))))
    

    (define/override (get-name) message)
    (define/override (checkout-arg) checksum)

    (define/override (checkout!)
      (super checkout!)
      (select-version-as-new!)
      (set-field! current-branch (root) #f)
      
      (send (root) set-headers! (make-buffer-string ((format "~a ~a" checksum message)
                                                     'font-lock-face 'gt:root-detached-face))))

    (define (call-with-head proc)
      (define current-branch (get-field current-branch (root)))
      (project-subprocess/void-output (format "git checkout ~a" checksum))
      (proc)
      (project-subprocess/void-output (format "git checkout ~a" (get-field name current-branch))))

    (define/public (branch-tag branch suffix)
      (call-with-head (λ () (project-subprocess/void-output
                             (format "git tag ~a-~a" (get-field name branch) suffix)))))

    (define/public (branch-untag branch suffix)
      (call-with-head (λ () (project-subprocess/void-output
                             (format "git tag -d ~a-~a" (get-field name branch) suffix)))))
    
    
    
    (define/public (init-message! v) (set! message v))
    (define/public (init-checksum! v) (set! checksum v))

    
    (define/public (mark-as-cv!)
      (put-extra-ftf-prop! ':foreground const:current-version-foreground-color))

    (define/public (mark-as-not-cv!)
      (remove-extra-ftf-prop! ':foreground))

    (define/public (select-version!)
      (mark-as-cv!)
      (set-field! current-version (root) this))

    (define/public (select-version-as-new!)
      (send (get-field current-version (root)) mark-as-not-cv!)
      (select-version!))
    
    
    ))



(define intr<%>
  (interface (descendant<%> ancestor<%> ebuffer:intr<%>)
    [merge! (->m void?)]
    
    ))


(define intr-mixin
  (mixin (descendant<%> ancestor<%> ebuffer:intr<%>) (intr<%>)
    (super-new)

    (field [merged? #f])

    (inherit-field parent name children)



    (inherit prev-leaf
             last-leaf
             root
             project-subprocess/void-output
             remove-from-tree!
             direct-call
             node-identifier
             branch-head)

    (define/override (repr-fields)
      (cons 'merged? (super repr-fields)))

    (define/override (equality-fields)
      (cons 'merged? (super equality-fields)))


    (define/override (field-setter-exprs)
      (cons `(set-field! merged? ,(node-identifier) ,merged?)
            (super field-setter-exprs)))

    (define/override (header-suffix)
      (send+ parent
             (header-suffix)
             (concat (make-buffer-string ((string-append "/" name) 'font-lock-face
                                          (cond [merged? 'sp:root-face]
                                                [else 'gt:root-unmerged-face]))))))
    
    (define/override (commitable?) (not merged?))

    
    (define/override (commit!)      
      (cond
       [(branch-head) => (λ (head)
                            (send head branch-untag this "unmerged-branch-end"))])
      (super commit!)
      (send (branch-head) branch-tag this "unmerged-branch-end"))


    (define/public (init-merged?! v) (set! merged? v))
    
    (define/public (merge!)
      (when (eq? (direct-call 'yes-or-no-p (format "merge ~a to ~a?"
                                                   name
                                                   (get-field name parent)))
                 't)
        (send* parent
          (checkout!)
          (project-subprocess/void-output (format "git merge ~a" name)))
        
        (set! merged? #t)
        (send* (root)
          (update-headers!)
          (update-current-version!))))

    
    (define/public (delete-branch!)
      (when merged?
        (error 'not-allowed-to-delete-merged-branch "~a" this))
      
      (when (eq? (direct-call 'yes-or-no-p (format "delete? ~a?" name))
                 't)
        (send parent checkout!)
        (remove-from-tree!)
        (project-subprocess/void-output (format "git branch -D ~a" name))
        (send (root) update-current-version!)))
      
      ))




(define root<%>
  (interface (ancestor<%> ebuffer:root<%>)
    ;; tofix use unit
    [initialize-repository! (->m any/c any/c)]
    
    [set-current-branch! (->m ancestor? void?)]
    [set-headers! (->m buffer-string? void?)]
    [update-headers! (->m void?)]
    [update-current-version! (->m void?)]

    [switch-to-git-tree-buffer! (->m void?)]
    [append-branch! (->m void?)]
    [checkout-master! (->m void?)]
    [parsed-git-log (->m (listof commit?))]
    ))

(define root-mixin
  (mixin (ancestor<%> ebuffer:root<%>) (root<%>)
    (super-new)
    
    (field [project-tree-root 'uninitialized]
           [current-branch 'uninitialized]
           [current-version 'uninitialized])
    
    (inherit-field name children)

    (set! name "master")
    

    (inherit get-backend
             append-child!
             last-leaf
             leafs
             tree-buffer
             checkout!
             node-identifier
             init-children!
             add-files!
             direct-call)
    


    (define/override (field-setter-exprs)
      (cons `(set-field! current-version ,(node-identifier) ,(send current-version node-identifier))
            (cons `(set-field! current-branch ,(node-identifier) ,(send current-branch node-identifier))
                  (super field-setter-exprs))))
    
    (define/override (commitable?) #t)
    
    (define/public (switch-to-git-tree-buffer!)
      (send (tree-buffer) switch-to-buffer!))



    (define/public (update-current-version!)
      (define head-checksum (car (regexp-match #px"^\\w+" (project-subprocess/string-output
                                                           "git log --oneline -n 1"))))
      (define head (findf (λ (node)
                             (equal? (get-field checksum node)
                                     head-checksum))
                          (leafs)))
      (send* head (select-version-as-new!) (select-as-new!)))


    (define/public (set-headers! header)
      (send (tree-buffer) set-header! header)
      (send+ project-tree-root (tree-buffer) (set-header! header)))

    (define/public (update-headers!)
      (set-headers! (send+ project-tree-root
                          (get-name-header)
                          (concat (send current-branch header-suffix)))))
    
    (define/public (set-current-branch! branch)
      (set! current-branch branch)
      (update-headers!))

    
    (define (make-git-ignore-commit)
      (define git-ignore-path (build-path (send project-tree-root absolute-path) ".gitignore"))
      
      (unless (file-exists? git-ignore-path)
        (call-with-output-file git-ignore-path void))
      
      (add-files! '(".gitignore"))
      
      (define checksum (second (regexp-match #px"(\\S+)\\]" 
                                             (project-subprocess/string-output
                                              (format "git commit -am 'initial commit'")))))
      (make (leaf%)
        [message "initial commit"]
        [checksum checksum]))
    
    
    (define/public (initialize-repository! project-root)
      (set! project-tree-root project-root)
      (set-current-branch! this)
      
      (unless (directory-exists? (build-path (send project-tree-root absolute-path) ".git"))
        (project-subprocess/void-output "git init"))
      
      
      (define first-commit
        (cond
         [(directory-exists? (build-path (send project-tree-root absolute-path) ".git"))
          (with-handlers ([exn:fail? (λ (e) (make-git-ignore-commit))])
            (match-define (list _ checksum message) (regexp-match #px"^(\\w+) (.+)\n$"
                                                                  (project-subprocess/string-output
                                                                   "git log --oneline -n 1")))
            (make (leaf%)
              [message message]
              [checksum checksum]))]
         
         [else (make-git-ignore-commit)]))
        
      
      (append-child! first-commit)      
      (send* first-commit
        (initialize-ebuffer-node!)
        (select!)
        (select-version!)))

    
    (define/override (project-subprocess/string-output command)
      (parameterize ([current-directory (send project-tree-root absolute-path)])
        (log-git-debug "exec: ~a" command)
        
        (define string-out (open-output-string))
        (parameterize ([current-output-port string-out]
                       [current-error-port string-out])
          (unless (system command)
            (error 'subprocess-error "~v\nout:~a" command
                   (get-output-string string-out))))
        
        (define output (get-output-string string-out))
        
        (log-git-debug "res: ~a" output)
        output))

    (define/override (project-subprocess/void-output command)
      (project-subprocess/string-output command) (void))
    
    (define/override (get-name) (send project-tree-root get-name))
    

    


    (define (intr%) (get-backend intr-container))
    (define (leaf%) (get-backend leaf-container)) 

    
    (define/public (infer! commits children-acc parent-node leafs-required)
      (let loop ([commits commits]
                 [children-acc children-acc]
                 [parent-node parent-node])
        (match commits
          [(cons x xs)
           
           (set! leafs-required (sub1 leafs-required))
           (cond
            [(and (< leafs-required 0) (eq? parent-node this))
             (init-children! children-acc)]
            [else 
             
             (match-define (commit checksum meta-seq message) x)
             (define leaf (make (leaf%)
                            [message message]
                            [checksum checksum]))
             
             (cond
              [meta-seq
               (match-define (meta head? start-tag end-tag) meta-seq)
               
               (when head?
                 (set! current-version leaf))
               
               (cond
                [end-tag
                 (define branch (make (intr%) [merged? (eq? end-tag 'merged)]))
                 (loop (loop xs
                             (list leaf)
                             branch)
                       (cons branch children-acc)
                       parent-node)]
                
                
                [start-tag
                 (send* parent-node
                   (init-children! children-acc)
                   (init-name! start-tag))
                 (set! leafs-required (add1 leafs-required))
                 (cons (struct-copy commit x
                                    [meta (struct-copy meta meta-seq
                                                       [start-tag #f])])
                       xs)]
                
                [else (loop xs (cons leaf children-acc) parent-node)])]
              [else  (loop xs (cons leaf children-acc) parent-node)])])]
          ['()
           (unless (eq? parent-node this)
             (error 'expected-root "~a" parent-node))
           (init-children! children-acc)])))

    
    
    (define/public (infer-tree!)
      (infer! (string-split (project-subprocess/string-output
                             "git -log --oneline --decorate --branches")
                            "\n")
              '()
              this))

    (define/public (parsed-git-log)
      (define-values (in out) (make-pipe))
      (parameterize ([current-directory (send project-tree-root absolute-path)]
                     [current-output-port out])
        (system "git log --oneline --decorate --branches")
        (close-output-port out))
      (begin0 (parse-log in)
        (close-input-port in)))
      
    
      
      


    (define/public (append-branch!)
      (send current-branch add-branch! (direct-call 'read-string "branch name: ")))

    (define/public (checkout-master!) (checkout!))
    
    
    (define/override (header-suffix)
      (make-buffer-string ("/master" 'font-lock-face 'sp:root-face)))
    
    
    ))




(define-composed-mixins
  [leaf-sum       (node descendant leaf)]
  [intr-sum       (node descendant ancestor intr)]
  [root-sum       (node ancestor root)])


(define-composed-mixins
  [leaf-final-sum (ebuffer:leaf-final-sum leaf-sum)]
  [intr-final-sum (ebuffer:intr-final-sum intr-sum)]
  [root-final-sum (ebuffer:root-final-sum root-sum)])


(define-inspected-class emulated-leaf% (class-from-mixins emulated-backend leaf-final-sum))
(define-inspected-class emulated-intr% (class-from-mixins emulated-backend intr-final-sum))
(define-inspected-class emulated-root% (class-from-mixins emulated-backend root-final-sum))


(define-inspected-class emacs-leaf% (class-from-mixins emacs-backend leaf-final-sum))
(define-inspected-class emacs-intr% (class-from-mixins emacs-backend intr-final-sum))
(define-inspected-class emacs-root% (class-from-mixins emacs-backend root-final-sum))

(define leaf-container (new backend-container%
                            [emulated emulated-leaf%]
                            [emacs emacs-leaf%]))

(define intr-container (new backend-container%
                            [emulated emulated-intr%]
                            [emacs emacs-intr%]))

