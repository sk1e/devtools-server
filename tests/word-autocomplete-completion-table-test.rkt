#lang racket/base

(require rackunit
         rackunit/text-ui
         
         "../word-autocomplete/completion-table.rkt"
         )

(define ht #hasheqv())

(define (completion-table-append! word)
  (set! ht (completion-table-append ht (list word))))

(define (completion-table-remove! word)
  (set! ht (completion-table-remove-one ht word)))

(define hey-table     (hasheqv #\h (completion-data (hasheqv #\e (completion-data (hasheqv #\y (completion-data (hasheqv) 1)) 0)) 0)))
(define hey+hey-table (hasheqv #\h (completion-data (hasheqv #\e (completion-data (hasheqv #\y (completion-data (hasheqv) 2)) 0)) 0)))

(define hey+hey+hello-table
  (hasheqv
   #\h
   (completion-data
    (hasheqv
     #\e
     (completion-data
      (hasheqv
       #\y
       (completion-data
        (hasheqv) 2)
       #\l
       (completion-data
        (hasheqv
         #\l
         (completion-data
          (hasheqv
           #\o
           (completion-data
            (hasheqv) 1)) 0)) 0)) 0)) 0)))

(define hey+hey+hello+dog-table
  (hasheqv
   #\d
   (completion-data
    (hasheqv
     #\o
     (completion-data
      (hasheqv
       #\g
       (completion-data
        (hasheqv) 1)) 0)) 0)
   #\h
   (completion-data
    (hasheqv
     #\e
     (completion-data
      (hasheqv
       #\y
       (completion-data
        (hasheqv) 2)
       #\l
       (completion-data
        (hasheqv
         #\l
         (completion-data
          (hasheqv
           #\o
           (completion-data
            (hasheqv) 1)) 0)) 0)) 0)) 0)))


(define completion-table-append-test
  (test-suite
   "completion-table-append test"

   (completion-table-append! "hey")   
   (test-equal? "hey" ht hey-table)
     
   (completion-table-append! "hey")
   (test-equal? "hey + hey" ht hey+hey-table)
   
   (completion-table-append! "hello")
   (test-equal? "hey + hey + hello" ht hey+hey+hello-table)
   
   (completion-table-append! "dog")      
   (test-equal? "hey + hey + hello + dog" ht hey+hey+hello+dog-table)))



(define complete-word-test
  (test-suite
   "complete-word test"
   (test-equal? "h" (complete-word "h" ht) '("hey" "hello"))
   (test-equal? "he" (complete-word "he" ht) '("hey" "hello"))
   (test-equal? "hey" (complete-word "hey" ht) '())
   (test-equal? "hel" (complete-word "hel" ht) '("hello"))
   (test-equal? "d" (complete-word "d" ht) '("dog"))
   ))

(define completion-table-remove-test
  (test-suite
   "completion-table-remove test"

      
   (completion-table-remove! "dog")
   (test-equal? "hey + hey + hello + dog - dog" ht hey+hey+hello-table)

   (test-exn "hey + hey + hello - cat" exn:fail:word-autocomplete:no-suffix? (lambda () (completion-table-remove! "cat")))

   (completion-table-remove! "hello")
   (test-equal? "hey + hey + hello - hello" ht hey+hey-table)


   (completion-table-remove! "hey")
   (test-equal? "hey + hey - hey" ht hey-table)

   (completion-table-remove! "hey")
   (test-equal? "hey - hey" ht #hasheqv())
   
   ))

(run-tests
 (test-suite
  "tt"
  completion-table-append-test
  complete-word-test
  completion-table-remove-test)
 )

