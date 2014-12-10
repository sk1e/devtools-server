#lang racket/base

(require ss/racket/class
         
         rackunit
         rackunit/text-ui

         "../backend/buffer.rkt")





(define (make-chunk value props end-point)
  (define chunk (new buffer-string-chunk%))
  (set-field! value chunk value)
  (set-field! properties chunk props)
  (set-field! end-point chunk end-point)
  chunk)


;; (define t (new buffer-string%))
;; (send t init-string! "qwe")


;; (define k (new buffer-string%))
;; (send k init-string! "asdasd")



;; (define q (new buffer-string%))
;; (send q init-string! "zxcc")

;; (send t concat k q)

(run-tests
 (test-suite
  "buffer test"

    
  ;(string-length (string-append "str1-" "str22-" "str3-" "str44-" "str555-" "str6-" "str7-"))

   (let ([ht-1 (make-immutable-hash (map cons
                               '(1 2 3)
                               '(4 5 6)))]

         [ht-2 (make-immutable-hash (map cons
                               '(1 2 3)
                               '(4 5 6)))]
         [ht-3 (make-immutable-hash (map cons
                               '(1 2 3)
                               '(4 5 1)))]
         [ht-4 (make-immutable-hash (map cons
                               '(1 2)
                               '(4 5)))]
         [ht-5 (make-immutable-hash (map cons
                               '(1 2 3 4)
                               '(4 5 6 7)))])
     (test-case
      "property-equal?"     
      (check-true (property-equal? ht-1 ht-1))
      (check-true (property-equal? ht-1 ht-2))
      (check-false (property-equal? ht-1 ht-3))
      (check-false (property-equal? ht-1 ht-4))
      (check-false (property-equal? ht-1 ht-5)))

     (test-case
      "property-update"
      (check-true (property-equal? (property-update ht-1 ht-3) ht-3))
      (check-true (property-equal? (property-update ht-4 ht-1) ht-2))
      (check-true (property-equal? (property-update ht-5 ht-1) ht-5)))
     )
     
  
  (test-case
   "multi chunk search"
   
   (let ([test-str (make-buffer-string "str1-" "str22-" "str3-" "str44-" "str555-" "str6-" "str7-")])
   
     
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 0) 0)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 1) 0)
     
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 5) 1)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 6) 1)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 10) 1)
     
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 11) 2)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 13) 2)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 15) 2)
     
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 16) 3)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 17) 3)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 21) 3)
     
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 22) 4)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 28) 4)
     
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 29) 5)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 33) 5)
     
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 34) 6)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 38) 6)))


  (test-case
   "single chunk search"

   (let ([test-str (make-buffer-string "single-str")])
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 0) 0)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 5) 0)
     (check-equal? (chunk-binary-search (get-field chunk-vector test-str) 11) 0)))


  
  

  (test-case
   "concat test"
   (let ([test-str (make-buffer-string "str1-" ("str2-" 'prop2 2))])
     (check-equal? (get-field chunk-vector test-str)
                   (vector (make-chunk "str1-" #hash() 5)
                           (make-chunk "str2-" #hash((prop2 . 2)) 10)))

     (check-equal? (get-field chunk-vector (send test-str concat
                                                 (make-buffer-string "str33-" ("str4-" 'prop4 4))
                                                 (make-buffer-string ("str5-" 'prop5 5) "str6")))
                   (vector (make-chunk "str1-" #hash() 5)
                           (make-chunk "str2-" #hash((prop2 . 2)) 10)
                           (make-chunk "str33-" #hash() 16)
                           (make-chunk "str4-" #hash((prop4 . 4)) 21)
                           (make-chunk "str5-" #hash((prop5 . 5)) 26)
                           (make-chunk "str6" #hash() 30)))))

  
  (let ([test-str (make-buffer-string ("str1" 'prop1 1)
                                      ("-str2" 'prop2 2)
                                      "-str3"
                                      ("-str4" 'prop3 3 'prop4 4))])
     
          
     (test-equal? "from+to substring"
                  (send test-str bs-substring 3 15)
                  (make-buffer-string ("1" 'prop1 1)
                                      ("-str2" 'prop2 2)
                                      "-str3"
                                      ("-" 'prop3 3 'prop4 4)))
   
   
     (test-equal? "from only substring"
                  (send test-str bs-substring 4)
                  (make-buffer-string ("-str2" 'prop2 2)
                                      "-str3"
                                      ("-str4" 'prop3 3 'prop4 4)))
   

   
     (test-equal? "first unpropertized substring"
                  (send (make-buffer-string "str1"
                                            ("-str2" 'prop1 1)
                                            ("-str3" 'prop2 2))
                        bs-substring 2 10)
                  (make-buffer-string "r1"
                                      ("-str2" 'prop1 1)
                                      ("-" 'prop2 2)))
     
     
    
     (test-equal? "last unpropertized substring"
                  (send test-str bs-substring 0 12)
                  (make-buffer-string ("str1" 'prop1 1)
                                      ("-str2" 'prop2 2)
                                      "-st"))
     
     
     (test-equal? "single prop substring"
                  (send (make-buffer-string "string1-"
                                            ("string-2" 'prop1 1))
                        bs-substring 0 10)
                  (make-buffer-string "string1-"
                                      ("st" 'prop1 1)))
     
     
     )
  
  (let ([test-first-str (make-buffer-string "first1-"
                                            ("first2-" 'prop1 1))])


    (define buffer (new emulated-buffer% [name "test buffer"]))

    (test-case
     "buffer first insert"
     (send buffer insert test-first-str)

     (check-equal? (get-field content buffer)
                   test-first-str))

    


    (test-case
     "buffer end insert"
     (send buffer insert (make-buffer-string ("end1-" 'prop1 1)
                                             "end2-"))
     (check-equal? (get-field content buffer)
                   (make-buffer-string "first1-"
                                       ("first2-" 'prop1 1)
                                       ("end1-" 'prop1 1)
                                       "end2-")))
    


    (test-case
     "buffer middle insert"
     (send buffer insert (make-buffer-string "mid1") #:point 13)


     (check-equal? (get-field content buffer)
                   (make-buffer-string "first1-"
                                       ("first" 'prop1 1)
                                       ("mid1")
                                       ("2-" 'prop1 1)
                                       ("end1-" 'prop1 1)
                                       "end2-")))

    (test-case
     "put-text-property"
     (send buffer put-text-property 1 9 'new-prop 'value)

     (check-equal? (get-field content buffer)
                   (make-buffer-string ("first1-" 'new-prop 'value)
                                      ("f" 'new-prop 'value 'prop1 1)
                                      ("irst" 'prop1 1)
                                      ("mid1")
                                      ("2-" 'prop1 1)
                                      ("end1-" 'prop1 1)
                                      "end2-")))

    
    )
  
  
  ;;(send buffer insert )
  
  
  
  
  
  

  
  ))


 ;; (make-buffer-string ("asdsd" prop 1)
 ;;                     "qwe")
