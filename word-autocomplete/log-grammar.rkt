#lang racket/base


(require racket/format
         racket/port
         racket/function
         racket/match
         racket/system
         
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))


(provide parse-log
         (struct-out commit)
         (struct-out meta))


(define-lex-abbrevs
  [hex-num (:/ "09" "af")]
  [word-char (:~ #\space #\~ #\^ #\: #\( #\) #\, #\newline)]      
  )

            
(define-tokens tokens (CHECKSUM WORD BRANCH-START))

(define-empty-tokens empty-tokens (NEWLINE
                                   EOF
                                   SPACE
                                   HEAD-MARKER
                                   META-SEPARATOR
                                   OP
                                   CP
                                   TAG-MARKER
                                   MERGED-BRANCH-END
                                   UNMERGED-BRANCH-END))


(define log-lexer
  (lexer
   [(eof) 'EOF]
   [""]
   ["(" 'OP]
   [")" 'CP]
   ["\n" 'NEWLINE]
   [" " 'SPACE]
   [(:= 7 hex-num) (token-CHECKSUM lexeme)]
   
   ["HEAD" 'HEAD-MARKER]
   ["tag: " 'TAG-MARKER]
   [", " 'META-SEPARATOR]
   [(:: (:+ word-char) "-branch-start") (token-BRANCH-START (substring lexeme
                                                                       0
                                                                       (- (string-length lexeme) 13)
                                                                       ))]
   
   [(:: (:+ word-char) "-merged-branch-end") 'MERGED-BRANCH-END]
   [(:: (:+ word-char) "-unmerged-branch-end") 'UNMERGED-BRANCH-END]
   [(:+ word-char) (token-WORD lexeme)]
   ))



(struct commit (checksum meta message) #:transparent)
(struct meta   (head? start-tag end-tag) #:transparent)


(define log-parser
  (parser

   (start log-lines)
   (end EOF)
   (tokens tokens empty-tokens )
   (error (lambda (a b c) (displayln (format "~a ~a ~a" a b c))))
      
   
   (grammar
    (log-lines [(log-line NEWLINE log-lines) (cons $1 $3)]
               [(log-line NEWLINE) (list $1)])

    [log-line [(CHECKSUM SPACE OP meta-data CP SPACE message) (commit $1 $4 $7)]
              [(CHECKSUM SPACE message) (commit $1 #f $3)]]
    
    [message [(WORD SPACE message) (string-append $1 " " $3)]
             [(WORD) $1]]
    

    [meta-data [(HEAD-MARKER) (meta #t #f #f)]
               [(HEAD-MARKER META-SEPARATOR rest-meta) (match $3
                                                         [(list start end) (meta #t start end)])]
               [(rest-meta) (match $1
                              [(list start end) (meta #f start end)])]]

    [rest-meta [(tags META-SEPARATOR post-tags) $1]
               [(tags) $1]
               [(post-tags) (list #f #f)]]
    
    
    [tags [(tags META-SEPARATOR branch-tag) $3]
          [(tags META-SEPARATOR word-tag) $1]
          [(branch-tag) $1]
          [(word-tag) '(#f #f)]]
    
    [branch-tag [(TAG-MARKER branch-tag-value) $2]]
    
    [branch-tag-value [(BRANCH-START) (list $1 #f)]
                      [(MERGED-BRANCH-END) '(#f merged)]
                      [(UNMERGED-BRANCH-END) '(#f unmerged)]]

    [word-tag [(TAG-MARKER WORD) #f]]

    [post-tags [(WORD META-SEPARATOR post-tags) #f]
               [(WORD) #f]]
    
    )))

;(const (values 1 2))

(define (parse-log input)
  (log-parser (λ () (log-lexer input))))


;; (root-from-log "(HEAD, master) cc-5"
;;                                "cc-4"
;;                                "(tag: bb-merged-branch-end) cc-3"
;;                                "cc-2"
;;                                "(tag: bb-branch-start) cc-1"
;;                                "cc-0")
;811f116 (HEAD, tag: qwe-3, tag: qwe-2, tag: ewq, tag: 2-c, tag: 12-c, tag: 1-a, tag: 0-d, subbranch-1, master, branch-1) sbcc-2
;811f116 (HEAD, tag: qwe-3, tag: qwe-2, tag: ewq, tag: 2-c, tag: 12-c, tag: 1-a, tag: 0-d, subbranch-1, master, branch-1) sbcc-2
;; (parse-log (open-input-string "bf3f3ee (qwe-b) qwec
;; b21a58a sbcc-1
;; 2f61491 bcc-2
;; 51629c4 bcc-1
;; 3caeeb5 cc-2
;; a451f99 cc-1\n"))

;; bf3f3ee (qwe-b) qwec\n811f116 (HEAD, tag: qwe-3, tag: qwe-2, tag: ewq, tag: 2-c, tag: 12-c, tag: 1-a, tag: 0-d, subbranch-1, master, branch-1) sbcc-2\nb21a58a sbcc-1\n2f61491 bcc-2\n51629c4 bcc-1\n3caeeb5 cc-2\na451f99 cc-1\n

;* a451f99 (tag: branch-1-branch-start, qwe) cc-2

;; (let cc ([v (open-input-string "* 3caeeb5 (HEAD)\n")])
;;   (define l (log-lexer v))
;;   (displayln l)
;;   (unless (eq? l 'EOF)
;;     (cc v)))


;; (let cc ([v (open-input-string "* 3caeeb5 (HEAD, tag: t-2, tag: t-1-branch-start, qwe) cc-2\n")])
;;   (define l (log-lexer v))
;;   (displayln l)
;;   (unless (eq? l 'EOF)
;;     (cc v)))

;; (define-values (in out) (make-pipe))
;; ;; (param)
;; ;; (display "123" out)
;; ;; (read-string  2 in)

