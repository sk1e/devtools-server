#lang racket/unit

(require racket/match
         racket/contract

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
    [find-node-by-chain (->m (listof string?) any)]
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
    
    (define/public (find-node-by-chain chain)
      (match children
        [(list _ ... (? (lambda (v)
                          (displayln (format ">>>>> ~a ~a" (send v get-name) (car chain)))
                          (equal? (car chain) (send v get-name))) next)
               _ ...)
         (match (cdr chain)
           ['() next]
           [xs (send next find-node-by-chain xs)])]))


    ))



(define-composed-mixins
  [ancestor-sum (node ancestor)])
