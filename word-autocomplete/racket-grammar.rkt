#lang racket/base


(require racket/format
         racket/port
         ss/racket/class
         
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)

         "tree/final.rkt")

(provide parse-module-cells
         parse-module)


(define-lex-trans make-num
  (syntax-rules ()
    ((_ digit radix exponent-marker)
     (:: (make-prefix radix) (make-complex digit exponent-marker)))))

(define-lex-trans make-prefix
  (syntax-rules ()
    ((_ radix) (:or (:: radix (:? exactness))
                    (:: (:? exactness) radix)))))

(define-lex-trans make-complex
  (syntax-rules ()
    ((_ digit exponent-marker)
     (:or (make-real digit exponent-marker)
          (:: (make-real digit exponent-marker) "@" (make-real digit exponent-marker))
          (:: (make-real digit exponent-marker) "+" (:or special-numbers (make-ureal digit exponent-marker)) i)
          (:: (make-real digit exponent-marker) "-" (:or special-numbers (make-ureal digit exponent-marker)) i)
          (:: (make-real digit exponent-marker) "+" i)
          (:: (make-real digit exponent-marker) "-" i)
          (:: "+" (:or special-numbers (make-ureal digit exponent-marker)) i)
          (:: "-" (:or special-numbers (make-ureal digit exponent-marker)) i)
          (:: "+" i)
          (:: "-" i)))))

(define-lex-trans make-ureal
  (syntax-rules ()
    ((_ digit exponent-marker)
     (make-ureal* digit exponent-marker make-suffix))))

(define-lex-trans make-ureal*
  (syntax-rules ()
    ((_ digit exponent-marker make-suffix)
     (:or (make-uinteger digit)
          (:: (make-uinteger digit) "/" (make-uinteger digit) (make-suffix digit exponent-marker))
          (make-decimal digit exponent-marker make-suffix)))))

(define-lex-trans make-real
  (syntax-rules ()
    ((_ digit exponent-marker)
     (make-real* digit exponent-marker make-suffix special-numbers))))

(define-lex-trans make-real*
  (syntax-rules ()
    ((_ digit exponent-marker make-suffix special-numbers)
     (:or (:: (:? sign) (make-ureal* digit exponent-marker make-suffix))
          (:: (char-set "+-") special-numbers)))))

(define-lex-trans make-uinteger
  (syntax-rules ()
    ((_ digit) (:: (:+ digit) (:* "#")))))

(define-lex-trans make-decimal
  (syntax-rules ()
    ((_ digit exponent-marker make-suffix)
     (:or (:: (make-uinteger digit) (make-suffix digit exponent-marker))
          (:: "." (:+ digit) (:* "#") (make-suffix digit exponent-marker))
          (:: (:+ digit) "." (:* digit) (:* "#") (make-suffix digit exponent-marker))
          (:: (:+ digit) (:+ "#") "." (:* "#") (make-suffix digit exponent-marker))))))

(define-lex-trans make-suffix
  (syntax-rules ()
    ((_ digit exponent-marker) (:or "" (:: exponent-marker (:? sign) (:+ digit))))))

(define-lex-trans make-extflonum-suffix
  (syntax-rules ()
    ((_ digit exponent-marker) (:: exponent-marker (:? sign) (:+ digit)))))

(define-lex-trans make-extflonum
  (syntax-rules ()
    ((_ digit radix)
     (:: radix (make-real* digit (:or "t" "T") make-extflonum-suffix special-extflonums)))))


(define-lex-abbrevs
  [a (char-set "aA")]
  [b (char-set "bB")]
  [c (char-set "cC")]
  [d (char-set "dD")]
  [e (char-set "eE")]
  [f (char-set "fF")]
  [g (char-set "gG")]
  [h (char-set "hH")]
  [i (char-set "iI")]
  [j (char-set "jJ")]
  [k (char-set "kK")]
  [l (char-set "lL")]
  [m (char-set "mM")]
  [n (char-set "nN")]
  [o (char-set "oO")]
  [p (char-set "pP")]
  [q (char-set "qQ")]
  [r (char-set "rR")]
  [s (char-set "sS")]
  [t (char-set "tT")]
  [u (char-set "uU")]
  [v (char-set "vV")]
  [w (char-set "wW")]
  [x (char-set "xX")]
  [y (char-set "yY")]
  [z (char-set "zZ")]
  
  [digit (:/ "0" "9")]
  [digit2 (:/ "0" "1")]
  [digit8 (:/ "0" "7")]
  [digit10 digit]
  [digit16 (:/ "af" "AF" "09")]
  
  [special-numbers (:or (:: n a n ".0") (:: i n f ".0")
                        (:: n a n ".f") (:: i n f ".f"))]
  [special-extflonums (:or (:: n a n ".t") (:: i n f ".t"))]
  [exponent-marker (:or e s f d l)]
  [exponent-marker16 (:or s l)]
  [sign (char-set "+-")]
  [exactness (:or "#i" "#e" "#I" "#E")]
  [radix2 (:or "#b" "#B")]
  [radix8 (:or "#o" "#O")]
  [radix10 (:or "#d" "#D")]
  [radix16 (:or "#x" "#X")]

  
  
  
  [number (:or (make-num digit2 radix2 exponent-marker)
               (make-num digit8 radix8 exponent-marker)
               (make-num digit10 (:? radix10) exponent-marker)
               (make-num digit16 radix16 exponent-marker16)
               (make-extflonum digit2 radix2)
               (make-extflonum digit8 radix8)
               (make-extflonum digit10 (:? radix10))
               (make-extflonum digit16 radix16))]


  [symbol-delims (:or (char-set "\",'`()[]{};") whitespace)]
  [symbol-chars (:~ symbol-delims "\\" "|")]
  [symbol-escapes (:or (:: "\\" any-char)
                           (:: "|" (:* (:~ "|")) "|"))]
  [symbol-start (:or symbol-escapes
                         (:~ symbol-delims "\\" "|" "#")
                         "#%")]
  [symbol (:: symbol-start
                  (:* symbol-escapes symbol-chars))]

  ;[symbol (:+ (:~  #\( #\) #\[ #\] #\{ #\} #\\ #\" #\, #\' #\` #\  #\newline #\tab))]
  
  [unicode  (:or (:: "u" (:** 1 4 digit16))
                 (:: "U" (:** 1 6 digit16)))]
  
  [character (:or (:: "#\\" any-char)
                  (:: "#\\" character-name)
                  (:: "#\\" (:/ "0" "3") digit8 digit8)
                  (:: "#\\" unicode))]
  
  [character-name (:or (:: s p a c e)
                       (:: n e w l i n e)
                       (:: n u l) 
                       (:: n u l l)
                       (:: b a c k s p a c e)
                       (:: t a b)
                       (:: l i n e f e e d)
                       (:: v t a b)
                       (:: p a g e)
                       (:: r e t u r n)
                       (:: r u b o u t))]

    
  [str-prefix (:or (:: "#" (:or "rx" "px") (:? "#"))
                   "#")]

  
  
  [string-word (:+ (:or (:~ "\"" "\\" #\newline #\space)
                        "\\\""
                        "\\\\"
                        "\\a"
                        "\\b"
                        "\\t"
                        "\\n"
                        "\\v"
                        "\\f"
                        "\\r"
                        "\\e"
                        "\\'"
                        (:: "\\" (:** 1 3 digit8))
                        (:: "\\x" (:** 1 2 digit16))
                        (:: "\\" #\newline)
                        (:: "\\" unicode)))]
  

  [space (:+ (:or blank #\newline ))]

  [line-comment (:: ";" (:* (:~ #\newline)))]
  
  [quote-abrv (:or "'" "`" "," ",@"
                   "#'" "#`" "#," "#,@")]
  
  [keyword (:: "#:" symbol)]

  (alphanum (:/ "az" "AZ" "09"))
  [langchar (:or (:/ "az" "AZ" "09") "+" "-" "_")]
  
  [lang-specifier (:: (:or "#lang " "#!")
                      (:or langchar
                           (:: langchar (:* (:or langchar "/")) langchar)))]
  
  )





            
(define-tokens value-tokens (NUMBER
                             CHAR
                             SPACE
                             SYMBOL
                             QUOTE
                             LINE-COMMENT
                             BLOCK-COMMENT-CONTENT
                             KEYWORD
                             HASH-OPEN
                             BOOLEAN
                             STRING-PREFIX
                             STRING-WORD
                             LANG-SPECIFIER))

(define-empty-tokens op-tokens (newline EOF DOT OSC BOX-OPEN STRING-QUOTES
                                        VOP VOCB VOSB   OP CP   OSB CSB   OCB CCB   OBC CBC))


(define (with-shifted-position port offset thunk)
  (file-position port (+ (file-position port) offset))
  (thunk))

(define block-lexer
  (lexer
   [(:: (complement (:: any-string (:or "#|" "|#") any-string)) (:or "#|" "|#"))
    (token-BLOCK-COMMENT-CONTENT (with-shifted-position input-port
                                                        -2
                                                        (λ ()
                                                           (substring lexeme
                                                                      0
                                                                      (- (string-length lexeme) 2)))))]
   ))


(define nc-balance 0)

(define nested-comment-lexer
  (lexer
   [(eof) 'EOF]
   
   ["#|" (begin (set! nc-balance (add1 nc-balance))
                'OBC)]
   
   ["|#" (begin (set! nc-balance (sub1 nc-balance))
                
                (when (zero? nc-balance)
                  (set! current-lexer main-lexer))
                
                'CBC)]
   
   ["" (block-lexer input-port)]

   ))


(define string-lexer
  (lexer
   [string-word (token-STRING-WORD lexeme)]
   [space (token-SPACE lexeme)]
   ["\"" (begin (set! current-lexer main-lexer)
                'STRING-QUOTES)]))

(define main-lexer
  (lexer
   [(eof) 'EOF]
   ["#(" 'VOP]
   ["(" 'OP]
   [")" 'CP]

   ["#[" 'VOSB]
   ["[" 'OSB]
   ["]" 'CSB]

   ["#{" 'VOCB]
   ["{" 'OCB]
   ["}" 'CCB]
   ["." 'DOT]
   
   
   ["\"" (begin (set! current-lexer string-lexer)
                'STRING-QUOTES)]
   
   ["#;" 'OSC]
   ["#&" 'BOX-OPEN]
   
   ["#|" (with-shifted-position input-port ; tofix, shift is unnecessary here
                                -2
                                (λ ()
                                   (set! current-lexer nested-comment-lexer)
                                   (nested-comment-lexer input-port)))]
   
   [(:: "#" (:or "hash" "hasheq" "hasheqv")) (token-HASH-OPEN lexeme)]
   [(:: "#" (:or "t" "T" "true"
                 "f" "F" "false")) (token-BOOLEAN lexeme)]
   
   [str-prefix (token-STRING-PREFIX lexeme)]
   [keyword (token-KEYWORD lexeme)]
   [quote-abrv (token-QUOTE lexeme)]
   [number (token-NUMBER lexeme)]
   [character (token-CHAR lexeme)]
   (line-comment (token-LINE-COMMENT lexeme))
   [space (token-SPACE lexeme)]
   [symbol (token-SYMBOL lexeme)]
   [lang-specifier (token-LANG-SPECIFIER lexeme)]
   ))


(define current-lexer main-lexer)

(define (racket-lexer input)
  (current-lexer input))

(define (list-maker %)
  (λ (children right-prefix left-suffix)
     (make %
       [children children]
       [right-prefix right-prefix]
       (left-suffix left-suffix))))

(define new-list (list-maker list%))
(define new-vector (list-maker vector%))
(define new-hash-pair (list-maker hash-pair%))

(define (new-hash children hash-type left right)
  (make hash%
    [children children]
    [left-prefix hash-type]
    [right-prefix left]
    (left-suffix right)
    ))


;; (define $module (parser-seq (parser-optional (parser-seq $lang-specifier
;;                                                          $blank))
;;                             (many $separator)
;;                             (parser-optional $sexps)
;;                             #:combine-with (compose (parent-constructor module%) flatten list)))


(define racket-parser
  (parser

   (start module-cells)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (displayln (format "~a ~a ~a" a b c))))
     
   
   (grammar

    ;; (main [(module-cells) (make module% [children $1])])
    
    (module-cells [(lang-specifier cells) (cons $1 $2)]
                  [(separators lang-specifier cells) (append $1 (list $2) $3)]
                  [(cells) $1])

    (lang-specifier [(LANG-SPECIFIER) (make lang-specifier% [name $1])])
    
    (block-comment-cell [(block-comment) $1]
                        [(BLOCK-COMMENT-CONTENT) (make block-comment-content% [name $1])])

    (block-comment-cells [(block-comment-cell block-comment-cells) (cons $1 $2)]
                         [(block-comment-cell) (list $1)])
    
    (block-comment [(OBC block-comment-cells CBC) (make block-comment% [children $2])])

    (sexp-comment [(OSC cell) (make sexp-comment% [children (list $2)])]
                  [(OSC separators cell) (make sexp-comment% [children (append $2 (list $3))])])
    
    (comment [(LINE-COMMENT) (make line-comment% [name $1])]
             [(block-comment) $1]
             [(sexp-comment) $1])

    (space [(SPACE) (make blank% [name $1])])
    
    (separator [(space) $1] 
               [(comment) $1])

    (separators [(separator separators) (cons $1 $2)]
                [(separator) (list $1)])

    (cell [(NUMBER) (make number% [name $1])]
          [(CHAR) (make char% [name $1])]
          [(BOOLEAN) (make boolean% [name $1])]
          [(SYMBOL) (make symbol% [name $1])]
          [(QUOTE) (make quote% [name $1])]
          [(KEYWORD) (make keyword% [name $1])]
          [(DOT) (make dot% [name "."])]
          [(string) $1]
          [(list) $1]
          [(vector) $1]
          [(hash) $1]
          [(box) $1])

       
    (cells [(separator cells) (cons $1 $2)]
           [(cell cells) (cons $1 $2)]
           [(cell) (list $1)]
           [(separator) (list $1)])

    (string [(STRING-PREFIX STRING-QUOTES words STRING-QUOTES) (make string%
                                                                 [children $3]
                                                                 [left-prefix $1])]
            [(STRING-QUOTES words STRING-QUOTES) (make string% [children $2])]
            [(STRING-QUOTES STRING-QUOTES) (make string% [children '()])]
            [(STRING-PREFIX STRING-QUOTES STRING-QUOTES) (make string%
                                                           [children '()]
                                                           [left-prefix $1])])
    
    (words [(separator words) (cons $1 $2)]
           [(word words) (cons $1 $2)]
           [(word) (list $1)]
           [(separator) (list $1)])
    
    (word [(STRING-WORD) (make string-word% [name $1])])
    
    
    (box [(BOX-OPEN cell) (make box% [children (list $2)])]
         [(BOX-OPEN separators cell) (make box% [children (append $2 (list $3))])])

    (list [(OP CP) (new-list '() "(" ")")]
          [(OSB CSB) (new-list '() "[" "]")]
          [(OCB CCB) (new-list '() "{" "}")]
          
          [(OP cells CP) (new-list $2 "(" ")")]
          [(OSB cells CSB) (new-list $2 "[" "]")]
          [(OCB cells CCB) (new-list $2 "{" "}")])
    
    
    (vector [(VOP CP) (new-vector '() "(" ")")]
            [(VOSB CSB) (new-vector '() "[" "]")]
            [(VOCB CCB) (new-vector '() "{" "}")]
            
            [(VOP cells CP) (new-vector $2 "(" ")")]
            [(VOSB cells CSB) (new-vector $2 "[" "]")]
            [(VOCB cells CCB) (new-vector $2 "{" "}")])

    
    (hash [(HASH-OPEN OP CP) (new-hash '() $1 "(" ")")]
          [(HASH-OPEN OSB CSB) (new-hash '() $1 "[" "]")]
          [(HASH-OPEN OCB CCB) (new-hash '() $1 "{" "}")]
     
          [(HASH-OPEN OP hash-pairs CP) (new-hash $3 $1 "(" ")")]
          [(HASH-OPEN OSB hash-pairs CSB) (new-hash $3 $1 "[" "]")]
          [(HASH-OPEN OCB hash-pairs CCB) (new-hash $3 $1 "{" "}")])

    
    (hash-pairs [(hash-pair hash-pairs) (cons $1 $2)]
                [(separator hash-pairs) (cons $1 $2)]
                [(hash-pair) (list $1)]
                [(separator) (list $1)])
    
    
    (hash-pair [(OP hash-pair-cells CP) (new-hash-pair $2 "(" ")")]
               [(OSB hash-pair-cells CSB) (new-hash-pair $2 "[" "]")]
               [(OCB hash-pair-cells CCB) (new-hash-pair $2 "{" "}")])

    
    (hash-pair-cells [(hash-pair-cell DOT hash-pair-cell) (append $1
                                                                  (cons (make dot% [name "."])
                                                                        $3))])

    (hash-pair-cell [(cell) (list $1)]
                    [(separators cell) (append $1 (list $2))]
                    [(cell separators) (cons $1 $2)]
                    [(separators cell separators) (append $1 (list $2) $3)])

    
    )))




(define (parse-module-cells input)
  (racket-parser (λ () (racket-lexer input))))

(define (parse-module input)
  (make syntax-root% (children (parse-module-cells input))))

;; (parse-src (open-input-string "'#hash((absolute-path? . #t)
;;          (arity-at-least? . #t))"))
;(parse-src (open-input-file "Projects/srcgraph/syntax/syntax-test.rkt"))
;(port->string (open-input-file "tt.rkt"))
;; (parse-src (open-input-string "(syntax->list #'(elem ...))"))
;(parse-src (open-input-string "#hash((#hash().\"asd\"))"))

;(define input (open-input-string (~v "qwe\"\"ewq")))
;(define input (open-input-string "(a b)"))
;(parse-src input)
;(make string% [name (~v "qwe\"\"ewq")])
;(ll input)
;(pp (λ () (ll input)))



#| |#

;; (let cc ([v (open-input-string "'#hash((absolute-path? . #t)
;; (arity-at-least? . #t))")])
;;   (define l (racket-lexer v))
;;   (displayln l)
;;   (unless (eq? l 'EOF)
;;     (cc v)))

;123 #| qwe |# ewq 

;(ellipsis? (car '(...)))
;(list ...)





