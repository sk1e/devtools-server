#lang racket/base

(require racket/unit         
         racket/contract

         ss/racket/class

         "node.rkt"
         "../base.rkt"
         "../file.rkt"
         "../ebuffer.rkt"
         "../../backend/emacs.rkt")



(define (ancestor? v) (is-a? v ancestor<%>))

  
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

;; (define (descendant? v) (is-a? v descendant<%>))


