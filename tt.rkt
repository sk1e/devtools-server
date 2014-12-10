#lang racket


(define cc
  (class object%
    (super-new)

    (define (qwe)
      (displayln "ff")
      (set! qwe (λ () (displayln "ss"))))

    (public qwe)

    
    ))
  




;; (qwe)
;; (qwe)
;; (qwe)

;(port->string in)

;(display eof)

;(get-output-string )




;;(display (read-string 1 in))

;; (define tag "tag: \\S+")
;; (define meta-rx (pregexp (string-append "^(HEAD, )?"
;;                                         (format "((?:~a, )*~a)?" tag tag)
;;                                         "((?:\\S+, )*(?:(?!master$)\\S+))?$"
;;                                         )))



;; (define a 1)
;; (define b 2)

;; (define c (list a b))

;; (set! b 1)



;; (define tag "tag: \\S+")
;; (define meta-rx (pregexp (string-append "^(HEAD, )?"
;;                                         (format "((?:~a, )*~a)?" tag tag)
;;                                         "((?:\\S+, )*\\S+)?$")))

;; (struct human ([name ])
;;         #:auto-value "---"
;;         #:transparent)

;; ;(human "qwe")
;; (human)

;; (remove "qwe" '("qwe"))
;; (regexp-match meta-rx
;;               "tag: branch-1-branch-start, master")

;(regexp-replace #rx"qwe" "asdqwe" "")
;(string-split "qwe" ", ")

;; (range 10 0)
;; (regexp-match (pregexp (format "((?:~a, )*~a)?" tag tag))
;;               "tag: master-start")

;; (regexp-match meta-rx
;;               "qwe, master")

;; (regexp-match #px"^a(?!qwe$)\\S+$" "aqwe")
;; (regexp-match #px"^a((?!qwe$)\\S+)?$" "aqwe")
;; (regexp-match #px"^a(?(?!qwe$)\\S+)?$" "aqwe")
;; (regexp-match #px"^a(?!qwe$)\\S+$" "aqwe")
;(regexp-match #px"(?<!qwe)e" "qwee")

;; (filter identity
;;         (map (curry regexp-match "^tag: (\\S+)-branch-start$")
;;              '("tag: master-branch-start")))

;(regexp-match #px"^tag: (\\S+)-branch-start$" "tag: master-branch-start")

;(string-split "tag: master-branch-start" ", ")

;(string-split "tag: master-branch-start, tag: qweqwe, tag: ololo" ", ")
;; (filter identity
;;         (map (curry regexp-match #px"tag: (\\S+)-branch-start")
;;              (string-split "tag: master-branch-start, tag: qweqwe, tag: ololo" ", ")))

;; (define (parse-git-log line)
;;   (match-define (list _ checksum meta message) (regexp-match line-rx line)))


;(map list '(1 2 3 4) '(2 3 4 5))



;; (require ss/racket/class)


;; (define a
;;   (class object%
;;     (super-new)
;;     (field [a (new b)])))


;; (define b
;;   (class object%
;;     (super-new)
;;     (field [b (new c)])))



;; (define c
;;   (class object%
;;     (super-new)
;;     (field [c 'c])))


;; (define qq (new a))

;; (get-field+ qq a b c)


;; (call-with-output-file "qwe"
;;   (curry write-string "ee ")
;;   #:exists 'append)

;; (call-with-input-file "qwe"
;;   port->string)


;; (require racket/list
;;          racket/function
;;          racket/match
;;          racket/string
;;          racket/set
;;          (only-in math/number-theory factorial))


;; (define (combinations set)
;;   (let calculate ([sx '()]
;;                   [sxs set])
;;     (match sxs
;;       ['() '()]
;;       [(cons x xs)
;;        (define comb (append sx (list x)))
;;        (cons comb (append (calculate comb xs)
;;                           (calculate sx xs)))])))



;; (define (shapley player-names coalition-worth-ht)
;;   (map (λ (name) (/ (apply + (map (λ (ordering)
;;                                      (define preceding-set (list->set (takef ordering (negate (curry equal? name)))))
;;                                      (- (hash-ref coalition-worth-ht (set-add preceding-set name))
;;                                         (hash-ref coalition-worth-ht preceding-set))
;;                                      )
;;                                   (permutations player-names)))
;;                     (factorial (length player-names))))
;;        player-names))


;; (define (calculate-shapley)
;;   (display "player names: ")
;;   (define player-names (string-split (read-line)))
;;   (define coalition-worth-ht (make-immutable-hash
;;                               (cons (cons (set) 0)
;;                                     (map (λ (names)
;;                                             (printf "v{~a}:" (string-join names ","))
;;                                             (cons (list->set names) (string->number (read-line))))
;;                                          (combinations player-names)))))
;;   (shapley player-names coalition-worth-ht))



;; (define (calculate-2x2-game-params)
;;   (display "a11-a22:")
;;   (match-define (list a11 a12 a21 a22) (map string->number (string-split (read-line))))
;;   (define c (+ (- a11 a12 a21) a22))
;;   (define v (/ (- (* a11 a22) (* a12 a21))
;;                c))
;;   (define x1 (/ (- a22 a21) c))
;;   (define x2 (/ (- a11 a12) c))
  
;;   (define y1 (/ (- a22 a12) c))
;;   (define y2 (/ (- a11 a21) c))
;;   (printf "v:~a\nx1:~a  x2:~a\ny1:~a  y2:~a\n" v x1 x2 y1 y2))



;; ;(calculate-2x2-game-params)
;; (calculate-shapley)

;; ;; (shapley '("1" "2" "3") (make-immutable-hash (list (cons (set "1") 0)
;; ;;                                                    (cons (set "2") 0)
;; ;;                                                    (cons (set "3") 0)
;; ;;                                                    (cons (set "1" "2") 0)
;; ;;                                                    (cons (set "1" "3") 1)
;; ;;                                                    (cons (set "2" "3") 1)
;; ;;                                                    (cons (set "1" "2" "3") 1)
;; ;;                                                    (cons (set) 0))))

;; ;(takef '(13 14 15 16) (negate (curry equal? 15)))

;; ;; (displayln (map list->set (permutations '("1" "2" "3"))))
;; ;; (displayln  (permutations '("1" "2" "3")))

;; ;(shapley (list (player "qwe" 1)))
