#lang racket/base

(require racket/unit
         racket/contract
         racket/match
         

         ss/racket/class

         "descendant-unit.rkt"
         "ancestor-unit.rkt"
         
         "ancestor-sig.rkt"

         "module.rkt"
         "runnable.rkt"
         "intr.rkt"
         "../base.rkt"
         "../file.rkt"
         "../ebuffer.rkt"
         "../../constants.rkt"
         "../../backend/emacs.rkt"
         "../../backend/buffer.rkt"
         "../../backend/buffer-string.rkt"
         (prefix-in completion: "../../word-autocomplete/completion-table.rkt")
         )


(provide root<%>
         root%)

(define-compound-unit/infer linked@
  (import )
  (export ancestor^)
  (link descendant@ ancestor@ )
  )


(define-values/invoke-unit/infer linked@)


(define root<%>
  (interface (base:quasiroot<%> file:intr<%> ancestor<%>)
    [init-file-buffers! (->m void?)]
    [cache-project! (->m void?)]
    ;; [init-test-directory-if-not! (->m void?)]
    
    [push-running-module! (->m (cons/c (is-a?/c runnable<%>) symbol?) void?)]
    [drop-running-module! (->m void?)]
    [current-running-module (->m (is-a?/c runnable<%>))]
    [run-other-if-left! (->m void?)]

    ;; [initialize-git-repository! (->m void?)]
    [get-name-header (->m buffer-string?)]
    [switch-to-current-project-node! (->m void?)]
    [complete-word (->m string? (listof string?))]
    ;; [reload-project! (->m void?)]
    ))



(define root-mixin
  (mixin (base:quasiroot<%> file:intr<%> ebuffer:root<%> ancestor<%>) (root<%>)
    (super-new)

    ;; (field [test-directory #f]
    ;;        ;; [git-root #f]
    ;;        )
    (field [shortcut-ht 'uninitialized])

    
    (inherit-field current-node name parent)
    
    (inherit leafs
             tree-buffer
             get-name
             cache-node!
             absolute-path
             node-identifier
             new-directory
             find-node-by-chain)

    
    ;; (define/override (field-setter-exprs)
    ;;   (cond
    ;;    [test-directory (cons `(set-field! test-directory ,(node-identifier) ,(send test-directory node-identifier))
    ;;                          (super field-setter-exprs))]
    ;;    [else (super field-setter-exprs)]))

    (define/public (init-shortcut-ht v)
      (set! shortcut-ht v))

    (define/override (next-neighbour) #f)
    
    (define/override (init-insert!)
      (send (tree-buffer) call
            `(lambda () (setq header-line-format (propertize ,(string-append " " (get-name))
                                                             'font-lock-face
                                                             '(:weight ultra-bold
                                                                       :height 110
                                                                       :family "Liberation Mono"
                                                                       :foreground "CornflowerBlue"))))))



    (define/override (pre-tree-insert!)
      (super pre-tree-insert!)
      (send (tree-buffer) set-header! (get-name-header)))
    ;; (send (tree-buffer) set-header! (cond
    ;;                                  [git-root (send (get-name-header) concat (send git-root header-suffix))]
    ;;                                  [else (get-name-header)])))
    
    (define/public (switch-by-shortcut! char-integer)
      (send+
       (find-node-by-chain (hash-ref shortcut-ht (integer->char char-integer)))
       (first-leaf)
       (select-as-new!)))

    (define/public (switch-to-current-project-node!)
      (send (get-field buffer current-node) switch-to-source-code-buffer!))
    
    (define/public (get-name-header)
      (make-buffer-string ((string-append " " (get-name))
                           'font-lock-face 'dt:root-face)))
    


    ;; (define/public (initialize-git-repository!)
    ;;   (set! git-root (new (git-root%)))
    ;;   (send git-root initialize-repository! this))
    
        
    (define/public (init-file-buffers!)
      (define leaf-nodes (leafs))
      (for-each (method initialize-project-node!) leaf-nodes)
      (send (car leaf-nodes) switch-to-source-code-buffer!))



    (define/public (cache-project!)
      (cache-node! (build-path (absolute-path) const:project-data-directory-name const:project-cache-file-name)))

    ;; (define/public (reload-project!)
    ;;   (for-each (compose (method kill!) (method leaf-buffer))
    ;;             (leafs))
    ;;   (send* parent
    ;;     (remove-project! this)
    ;;     (load-project! (path->string name))))

    (define/override (project-path) 
      (build-path "/" name))
    
    (define/override (mode-line-path) "/")
    ;; (define/public (init-test-directory-if-not!)
    ;;   (unless test-directory
    ;;     (set! test-directory (make test-directory% [name "tests"]))
    ;;     (add-project-node! test-directory)
    ;;     (send test-directory make-directory-if-not!)))


    
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
        [(cons (cons module-node executor) _) (send module-node run-start! executor)]))



    (define completion-table 'uninitialized)
    
    (define/public (init-word-autocomplete!)
      (define ht (make-hash))
      (for ([leaf (leafs)])
        (match (regexp-match #px"\\.[\\w]+$" (send leaf get-name))
          [(list x)
           (send leaf parse-words!)
           (hash-update! ht x
                         (lambda (v) (cons leaf v)) (lambda () '()))]))


      (define ht-list (hash->list ht))
      (set! completion-table
            (for/hash ([extension (map car ht-list)]
                       [leafs (map cdr ht-list)])
              (values extension (map (compose completion:make-completion-table (field-getter words))
                                     leafs))))
      
      )

    (define/public (complete-word prefix)
      (match (regexp-match #px"\\.[\\w]+$" (send current-node get-name))
        [(list x) (completion:complete-word prefix (hash-ref x completion-table))]
        [_ '()]))
    
    ))


(define-inspected-class root% (class-from-mixins ebuffer:quasiroot-final-sum
                                                 file:intr-sum ancestor-sum
                                                 root
                                                 child-directory
                                                 child-file))
