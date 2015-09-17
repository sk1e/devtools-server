#lang racket/base

(require racket/vector
         racket/match)

(provide (struct-out buffer-string)
         
         bs-append
         bs-length
         bs-substring
         bs-make)

;; (define (hash->list))

(define (hash->plain-list h)
  (reverse (foldl (λ (x res)
                     (cons (cdr x) (cons (car x) res)))
                  '()
                  (hash->list h))))



(define (write-bs bs port mode)
  (write-string (format "#(~s" (buffer-string-content bs)) port)

  (define props (vector->list (buffer-string-properties bs)))
  (let loop ([idx 1]
             [start-idx 0]
             [prev (car props)]
             [props (cdr props)])
    (match props
      [(cons x xs) (cond
                    [(equal? x prev) (loop (add1 idx)
                                           start-idx
                                           prev
                                           xs)]
                    [else (unless (hash-empty? prev)
                            (write-string (format " ~a ~a ~s" start-idx idx (hash->plain-list prev)) port))
                          (loop (add1 idx)
                                idx
                                x
                                xs)])]
      ['() (unless (hash-empty? prev)
             (write-string (format " ~a ~a ~s" start-idx idx (hash->plain-list prev)) port))]))
  (write-string ")" port))

  
  


(struct buffer-string (content properties)
        #:transparent
        #:methods gen:custom-write
        [(define write-proc write-bs)])


(define (bs-append . xs)
  (foldl (λ (x res)
            (buffer-string (string-append (buffer-string-content res)
                                          (buffer-string-content x))
                           (vector-append (buffer-string-properties res)
                                          (buffer-string-properties x))))
         (buffer-string "" #())
         xs))


(define (bs-length x)
  (string-length (buffer-string-content x)))


(define (bs-substring x from [to (bs-length x)])
  (buffer-string (substring (buffer-string-content x) from to)
                 (vector-copy (buffer-string-properties x) from to)))


(define (bs-make str props)
  (buffer-string str
                 (make-vector (string-length str)
                              (make-immutable-hasheq props))))



;; (bs-append (bs-make "q" '((ff . (:foreground "red"))))
;;            (bs-make "qwe" '((ff . (:foreground "blue"))))
;;            (bs-make "asd" '())
;;            (bs-make "qwe" '((ff . (:foreground "red"))))
;;            ;; (bs-make "qwe" '((ff . (:foreground "blue"))))
;;            ;; (bs-make "zxc" '())
;;            )

;; (~s (bs-append (bs-make "asd" '())
;;            (bs-make "qwe" '((ff . (:foreground "red"))))
;;            (bs-make "zxc" '())))

;(write (bs-make "qwe" '()))





