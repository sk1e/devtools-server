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
    ;; [push-project-node!          (->m (implementation?/c ebuffer:descendant<%>) void?)]
    ;; [append-project-node!        (->m (implementation?/c descendant<%>) void?)]
    ;; [push-project-node/select!   (->m (implementation?/c base:descendant<%>) void?)]
    ;; [append-project-node/select! (->m (implementation?/c base:descendant<%>) void?)]
    ))



(define ancestor-mixin
  (mixin (node<%> ebuffer:ancestor<%> file:ancestor<%>) (ancestor<%>)
    (super-new)

    (inherit-field children)

    (inherit push-child!
             append-child!)

    (define (init-project-node! node)
      (send* node
        (initialize-ebuffer-node!)
        (initialize-project-node!)))
    
    (define/public (push-project-node! node)
      (push-child! node)
      (init-project-node! node))

    (define/public (append-project-node! node)
      (append-child! node)
      (init-project-node! node))
    


    (define/public (push-project-node/select! node)
      (push-project-node! node)
      (send node select-as-new!))

    (define/public (append-project-node/select! node)
      (append-project-node! node)
      (send node select-as-new!))

    ))



(define-composed-mixins
  [ancestor-sum (node ancestor)])
