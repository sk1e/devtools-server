#lang racket/base

(require racket/contract
         racket/match
         racket/string

         ss/racket/class
         
         "leaf.rkt"
         ;; "intr.rkt"
         "runnable.rkt"
         "test.rkt"
         "../file.rkt"
         "../../constants.rkt"
         "../../utils/path.rkt"
         )

(provide module<%>
         module%
         racket-module%
         child-file-mixin)

(define module<%>
  (interface (runnable<%>)
    [init-test-module (->m (is-a?/c file:leaf<%>) void?)]
    [test-name (->m string?)]
    [create-test! (->m void?)]
    [toggle-test! (->m void?)]

    [run-test-at-background! (->m void?)]
    ))

(define table (make-hash))

(define/contract (register-module%! module extension)
  (-> (implementation?/c module<%>) string? void?)
  (hash-set! table extension module))

(define/contract (get-module% extension)
  (-> string? (or/c (implementation?/c module<%>) #f))
  (hash-ref table extension (lambda () #f)))


(define module-mixin
  (mixin (runnable<%>) (module<%>)
    (field [test-module #f])
    
    (super-new)

    (inherit-field indicators
                   parent)
    
    (inherit project-path
             root
             buffer-name
             node-identifier
             switch-to-source-code-buffer!
             test-indicator
             run!
             get-name)
    
    (define/override (field-setter-exprs)
      (cond
       [test-module (cons `(send ,(node-identifier) init-test-module ,(send test-module node-identifier))
                          (super field-setter-exprs))]
       [else (super field-setter-exprs)]))
        

        
    (define/public (init-test-module v)
      (set! test-module v)
      (set-field! tested-module test-module this)
      (send test-module initialize-project-node!))
    
    (define/public (test-name)
      (string-append (string-join (match (map path->string (cddr (path->list (project-path))))
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
      (void)
      ;; (define test-dir (make directory% [name "tests"]))      
      ;; (send parent push-child! test-dir)
      ;; (send test-dir make-directory-if-not!)
      ;; (set! test-module (make test% [name (get-name)]))
      ;; (set-field! tested-module test-module this)
      ;; (send test-dir push-child! test-module)
      ;; (send test-module initialize-project-node!)
      ;; (send (test-indicator) switch-on!)
      ;; (send (get-field buffer test-module) switch-to-buffer!)



      
      ;; (send (root) init-test-directory-if-not!)
      
      ;; (define test-dir (get-field test-directory (root)))
      ;; (init-test-module (make module-test% [name (test-name)]))
      
      ;; (send test-dir add-project-node! test-module)
      ;; (send (test-indicator) switch-on!)

      ;; (send test-module make-file-if-not!)
      )

    

    (define/public (toggle-test!)
      (when test-module
        (send test-module select-as-new!)))

    
    (define/public (run-test-at-background!)
      (send test-module run! 'pt:execute-test-at-background))

    (define/override (run-module-at-foreground!)
      (run! 'pt:execute-module-at-foreground))


    ))


(define child-file-mixin
  (mixin (file:ancestor<%>) ()
    (super-new)

    (define/override (child-file% name)
      (match (regexp-match #px"\\.[\\w]+$" name)
        [(list extension) (or (get-module% extension) file%)]
        [_ file%]))))

;; (define child-directory-mixin
;;   (mixin (file:ancestor<%>) ()
;;     (super-new)

;;     (define/override (child-directory%) directory%)))



(define-inspected-class module% (class-from-mixins runnable-final-sum module))

(define-inspected-class racket-module%
  (class module%
    (super-new)))

(register-module%! racket-module% ".rkt")



