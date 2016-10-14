#lang racket/unit

(require racket/match

         ss/racket/class
         
         "descendant-sig.rkt"
         "ancestor-sig.rkt"

         "node.rkt"
         
         "../ebuffer.rkt"
         "../file.rkt")

(import descendant^)
(export ancestor^)


(define ancestor<%>
  (interface (node<%>)
    [add-project-node! (->m (is-a?/c descendant<%>) void?)]))



(define ancestor-mixin
  (mixin (node<%> ebuffer:ancestor<%> file:ancestor<%>) (ancestor<%>)
    (super-new)

    (inherit-field children)

    (inherit push-child!)

    ;; (define/override (child-directory%) directory%)

    ;; (define/override (child-file% name)
    ;;   (match name
    ;;     [(regexp #rx"[.]rkt$") module%]
    ;;     [_ file%]))
    
    (define/public (add-project-node! node)
      (push-child! node)
      (send* node
        (initialize-ebuffer-node!)
        (initialize-project-node!)))


    (define/public (add-project-node/select! node)
      (add-project-node! node)
      (send node select-as-new!))

    ))



(define-composed-mixins
  [ancestor-sum (node ancestor)])
