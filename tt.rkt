#lang racket/base


;; (foldl (λ (x res)
;;           (printf "~a ~a\n" x res)
;;           (+ x res))
;;        0
;;        '(1 2 3 4))

;; (define (pp v p [d 0])
;;   (print "asd"))

;; (parameterize [(global-port-print-handler pp)]
;;   (print "de"))



;; [vector-copy (All (a) (->* ((Vectorof a)) (Integer Integer) (Vectorof a)))]

;; (vector-append (vector 1)
;;                (vector 2))



;; (require racket/draw)


;; (define indicator<%>
;;   (interface ()

    
;;     [render! (->m (is-a?/c bitmap-dc%) string? void?)]

    
;;     ))


;; (define-namespace-anchor anchor)
;; (define ns (namespace-anchor->namespace anchor))

;; (eval '(new object%)
;;       ns)


