#lang racket/base

(require racket/contract)
(provide path->list)

(define/contract (path->list path)
  (-> path-string? (listof path?))
  (let splitted ([p path]
                 [acc '()])
    (define-values (base name _) (split-path p))
    (cond
     [base (splitted base (cons name acc))]
     [else (cons name acc)])))
