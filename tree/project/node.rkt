#lang racket/base

(require racket/class
         racket/contract
         
         "../file.rkt"
         "../ebuffer.rkt"
         "../../backend/emacs.rkt")


(provide node<%>
         node-mixin)


(define node<%>
  (interface (file:node<%> ebuffer:node<%>)
    [project-path (->m path-string?)]
    [mode-line-path (->m path-string?)]
    ))

(define node-mixin
  (mixin (file:node<%> ebuffer:node<%>) (node<%>)
    (super-new)
    
    (inherit-field name)


    (abstract project-path
              mode-line-path)

    (define/override (get-name) (path->string name))
    
    (define/override (tree-buffer) (get-field project-buffer (emacs)))

    ))




