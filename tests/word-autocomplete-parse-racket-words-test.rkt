#lang racket/base

(require ss/racket/class

         
         rackunit
         rackunit/text-ui
         
         "../word-autocomplete/parse-racket-words.rkt"
         )


(define (parse-words string)
  (parse-racket-words (open-input-string string)))

(run-tests
 (test-suite
  "lisp word grammar test"

  
  (test-equal? "one word"
               (parse-words "word")
               '("word"))

  (test-equal? "whitespace separated"
               (parse-words "word1 word2")
               '("word1" "word2"))
  
  (test-equal? "simple sexp"
               (parse-words "(some-word1 word2)")
               '("some-word1" "word2"))

  (test-equal? "sexp with cap and string"
               (parse-words "(Some-word1 \"word2\")")
               '("Some-word1"))

  
  

  ))

