#lang racket/base


(require racket/contract
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (contract-out
          [parse-racket-words (-> input-port? (listof string?))]))



(define-lex-abbrevs
  [hex-num (:/ "09" "af")]
  [word (:: alphabetic (:* (:or alphabetic numeric "-")))]
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


(define string-lexer
  (lexer
   ["\\\"" (string-lexer input-port)]
   ["\"" (racket-words-lexer input-port)]
   [any-char (string-lexer input-port)]))

(define racket-words-lexer
  (lexer
   [(eof) 'EOF]
   [word (token-WORD lexeme)]
   ["\"" (string-lexer input-port)]
   [any-char (racket-words-lexer input-port)]
  
   ))



(define racket-words-parser
  (parser

   (start racket-words)
   (end EOF)
   (tokens tokens empty-tokens)
   (error (lambda (a b c) (displayln (format "~a ~a ~a" a b c))))
      
   
   (grammar
    (racket-words [(WORD racket-words) (cons $1 $2)]
                  [(WORD) (list $1)])
    
    )))



(define (parse-racket-words input)
  (racket-words-parser (λ () (racket-words-lexer input))))

