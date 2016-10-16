#lang typed/racket/base

(require racket/match
         racket/list)

(provide make-completion-table
         completion-table-append
         completion-table-remove-one
         complete-word
         (struct-out completion-data)
         (struct-out exn:fail:word-autocomplete)
         (struct-out exn:fail:word-autocomplete:no-suffix)
         (struct-out exn:fail:word-autocomplete:bad-entries-number))
                     
                    




(struct completion-data ([ht : Completion-table]
                         [entries-number : Natural])
        #:transparent)

(define-type Completion-table (HashTable Char completion-data))


(struct exn:fail:word-autocomplete exn:fail ())
(struct exn:fail:word-autocomplete:no-suffix exn:fail:word-autocomplete ([word : String]
                                                                         [suffix : String]))

(struct exn:fail:word-autocomplete:bad-entries-number exn:fail:word-autocomplete ([word : String]
                                                                                  [entries-number : Natural]))




(: make-completion-table ((Listof String) -> Completion-table))
(define (make-completion-table strings)
  (completion-table-append #hasheqv() strings))

(: completion-table-append (Completion-table (Listof String) -> Completion-table))
(define (completion-table-append ht strings)
  (for/fold : Completion-table ([completions-table ht])
            ([string (in-list strings)])
            
            (let loop : Completion-table ([chars : (Listof Char) (string->list string)]
                       [char-table completions-table])
              (match chars
                [(cons x xs)
                 (define data (hash-ref char-table x (lambda () (completion-data #hasheqv() 0))))
                 (hash-set char-table x
                           (cond
                            [(null? xs)
                             (struct-copy completion-data data
                                          [entries-number (add1 (completion-data-entries-number data))])]
                            [else
                             (struct-copy completion-data data
                                          [ht (loop xs (completion-data-ht data))])]))]

                ['() char-table]))))





(: complete-word (String Completion-table  -> (Listof String)))
(define (complete-word prefix ct)

  (: collect-words (Completion-table (Listof Char) (Listof String) -> (Listof String)))
  (define (collect-words ct chars words)
    (match (hash-keys ct)
      [(cons x xs) (foldl (lambda ([x : Char] [res : (Listof String)]) (collect-words-for-key ct x chars res))
                          (collect-words-for-key ct x chars words)
                          xs)]
      ['() words]))


  (: collect-words-for-key (Completion-table Char (Listof Char) (Listof String) -> (Listof String)))
  (define (collect-words-for-key ct char chars words)
    (match-define (completion-data ht entries-number) (hash-ref ct char))
       (match entries-number
         [0 (collect-words ht (cons char chars) words)]
         [_ (collect-words ht (cons char chars) (cons (string-append prefix (list->string (reverse (cons char chars))))
                                                      words))]))
  
  (let loop : (Listof String) ([ct : Completion-table ct]
                               [chars : (Listof Char) (string->list prefix)])
       (match chars
         [(cons x xs) (cond
                       [(hash-has-key? ct x) (loop (completion-data-ht (hash-ref ct x)) xs)]
                       [else '()])]
         ['() (collect-words ct '() '())])))






(: completion-table-remove-one (Completion-table String -> Completion-table))
(define (completion-table-remove-one top-ct word)
  (let loop : Completion-table ([chars : (Listof Char) (string->list word)]                                
                                [temp-acc : (Listof Char) '()]
                                [update-ct : Completion-table top-ct]
                                [ct : Completion-table top-ct])
       (match chars
         [(cons x xs)
          (cond
           [(not (hash-has-key? ct x)) (raise (exn:fail:word-autocomplete:no-suffix
                                               (format "completion-table-remove: no suffix for word in completion-table\nword: ~a\nsuffix: ~a" word (list->string chars))
                                               (current-continuation-marks)
                                               word
                                               (list->string chars)))]
           [else
            (define data (hash-ref ct x))
            (match xs
              ['() (match (completion-data-entries-number data)
                     [1  (hash-remove update-ct (match temp-acc
                                                  ['() x]
                                                  [_ (last temp-acc)]))]
                     [_ (completion-table-update-entries update-ct (reverse (cons x temp-acc)) word)]
                     )]
              [_ (match (hash-count (completion-data-ht (hash-ref ct x)))
                   [1 (loop xs (cons x temp-acc) update-ct (completion-data-ht (hash-ref ct x)))]
                   [_ (completion-table-update update-ct
                                               (reverse (cons x temp-acc))
                                               (loop xs
                                                     '()
                                                     (completion-data-ht (hash-ref ct x))
                                                     (completion-data-ht (hash-ref ct x))))])])])]
         )))

(: completion-table-update (Completion-table (Listof Char) Completion-table -> Completion-table))
(define (completion-table-update ct update-chars new-ct)
  (let loop : Completion-table ([ct : Completion-table ct]
                                [chars : (Listof Char) update-chars])
       (match chars
         [(cons x xs) (hash-update ct x (lambda (data)
                                          (struct-copy completion-data data
                                                       [ht (loop (completion-data-ht (hash-ref ct x)) xs)])))]
         ['() new-ct])))

(: completion-table-update-entries (Completion-table (Listof Char) String -> Completion-table))
(define (completion-table-update-entries ct update-chars word)
  (let loop : Completion-table ([ct : Completion-table ct]
                                [chars : (Listof Char) update-chars])
       (match chars
         [(list x) (hash-update ct x (lambda (data) (match data
                                                 [(completion-data ht (? exact-positive-integer? entries-number)) (completion-data ht (sub1 entries-number))]
                                                 [(completion-data _ entries-number)
                                                  (raise (exn:fail:word-autocomplete:bad-entries-number
                                                          (format "completion-table-remove: bad entries number for word in completion-table\nword: ~a\nentries-number: ~a" word entries-number)
                                                          (current-continuation-marks)
                                                          word
                                                          entries-number))])
                                        ))]
         [(cons x xs) (hash-update ct x (lambda (data)
                                          (struct-copy completion-data data
                                                       [ht (loop (completion-data-ht (hash-ref ct x)) xs)])))])))



