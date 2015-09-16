#lang racket/base

(require ss/racket/class
         
         rackunit
         rackunit/text-ui
         
         "../backend/buffer.rkt"
         "../backend/buffer-string.rkt"
         )






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


  
  (let ([test-str (make-buffer-string ("str1" 'prop1 1)
                                      ("-str2" 'prop2 2)
                                      "-str3"
                                      ("-str4" 'prop3 3 'prop4 4))])
     
          
     (test-equal? "from+to substring"
                  ;; (send test-str bs-substring 3 15)
                  (bs-substring test-str 3 15)
                  (make-buffer-string ("1" 'prop1 1)
                                      ("-str2" 'prop2 2)
                                      "-str3"
                                      ("-" 'prop3 3 'prop4 4)))
   
   
     (test-equal? "from only substring"
                  ;; (send test-str bs-substring 4)
                  (bs-substring test-str 4)
                  (make-buffer-string ("-str2" 'prop2 2)
                                      "-str3"
                                      ("-str4" 'prop3 3 'prop4 4)))
   

   
     (test-equal? "first unpropertized substring"
                  (bs-substring (make-buffer-string "str1"
                                                    ("-str2" 'prop1 1)
                                                    ("-str3" 'prop2 2))
                                2 10)
                  ;; bs-substring 2 10)
                  (make-buffer-string "r1"
                                      ("-str2" 'prop1 1)
                                      ("-" 'prop2 2)))
     
     
    
     (test-equal? "last unpropertized substring"
                  ;; (send test-str bs-substring 0 12)
                  (bs-substring test-str 0 12)
                  (make-buffer-string ("str1" 'prop1 1)
                                      ("-str2" 'prop2 2)
                                      "-st"))
     
     
     (test-equal? "single prop substring"
                  (bs-substring (make-buffer-string "string1-"
                                                    ("string-2" 'prop1 1))
                                0 10)
                        ;; bs-substring 0 10)
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
