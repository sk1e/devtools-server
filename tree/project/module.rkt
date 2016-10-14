#lang racket/base

(require racket/unit
         racket/contract
         racket/match
         racket/string

         ss/racket/class
         
         "descendant-sig.rkt"
         "ancestor-sig.rkt"
         
         "descendant-unit.rkt"
         "ancestor-unit.rkt"         
         

         "leaf.rkt"
         "runnable.rkt"
         "test.rkt"
         "../file.rkt"
         "../../constants.rkt"
         )

(provide module<%>
         module%
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
      (string-append (string-join (match (map path->string (cddr (file:path->list (project-path))))
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


(define child-file-mixin
  (mixin (file:ancestor<%>) ()
    (super-new)

    (define/override (child-file% name)
      (match (regexp-match #px"\\.[\\w]+$" "qwe.ds")
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



