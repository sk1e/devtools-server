#lang racket/base

(require racket/contract
         
         ss/racket/class
         
         "../../word-autocomplete/parse-racket-words.rkt"
         "../../backend/emacs.rkt"
         )

(provide word-autocompletable<%>
         word-autocompletable-mixin
          )


(define word-autocompletable<%>
  (interface ()
    ))


(define word-autocompletable-mixin
  (mixin () (word-autocompletable<%>)
    (super-new)

    (inherit-field buffer)
    
    (field [words 'uninitialized])
    
    (define/public (parse-words!)
      (set! words (parse-racket-words (open-input-string (send buffer get-content)))))
    
    ))


