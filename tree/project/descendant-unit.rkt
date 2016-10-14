#lang racket/unit

(require  racket/contract
          
          ss/racket/class
          
         "descendant-sig.rkt"
         "ancestor-sig.rkt"

         "node.rkt"
         
         "../ebuffer.rkt"
         "../file.rkt"
         "../../backend/emacs.rkt")

(import ancestor^)
(export descendant^)

(define (ancestor? x) (is-a? x ancestor<%>))

(define descendant<%>
  (interface (node<%> ebuffer:descendant<%>)
    [entered-directory (-> ebuffer:current-node? ancestor?)]
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
      (send (entered-directory) add-project-node/select! new-file))
    
    
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


(define-composed-mixins
  [descendant-sum (node descendant)])



