#lang racket/base

(require racket/class
         racket/pretty
         racket/string
         
         "epc.rkt"
         
         (for-syntax racket/base
                     racket/function
                     
                     ss/racket/syntax
                     
                     syntax/parse
                     syntax/stx)
         )

(provide (all-defined-out)
         ;epc-logger
         )

;(define sg-logger (make-logger 'srcgraph-logger epc-logger))
;(current-logger sg-logger)

(define server (new server%))


(define (el-deferred-call procedure-symbol . args)
  (send server append-call procedure-symbol args))

(define (el-direct-call procedure-symbol . args)
  (send server call-remote-procedure procedure-symbol args))





;; (define-syntax (el-insert stx)
;;   (syntax-parse stx
;;     [(_ text
;;         (~or (~optional (~seq #:point point:expr))
;;              (~optional (~seq #:buffer buffer:expr))
;;              (~optional (~seq #:face face:expr)))
;;         ...)

     

     
;;      (with-syntax* ([text-expr (if (attribute face)
;;                                    #'(insert (propertize ,text 'font-lock-face ',face))
;;                                    #'(insert ,text))]
;;                     [(text+?point ...) (if (attribute point)
;;                                            #'((goto-char ,point) text-expr)
;;                                            #'(text-expr))]
;;                     [body (if (attribute buffer)
;;                               #'(with-current-buffer ,buffer
;;                                                      text+?point ...)
;;                               #'(progn text+?point ...))])
;;                    #'(el-call `(lambda () body))
                   
;;                    )]))




;; (define-syntax-rule (el-insert args ...)
;;   (el-call 'eval (el-insert-form args ...)))

;; (map string->symbol (string-split (symbol->string (syntax-e #'main.sub.subber))
;;                                   "."))

;; (define-syntax get-attributes
;;   (syntax-rules ()
;;     [(_ obj attr) (get-field attr obj)]
;;     [(_ obj attr1 attr2 ...) (get-attributes (get-field attr1 obj) attr2 ...)]))

;; (pretty-display (syntax->datum (expand #'(get-attributes current-project current-leaf point))))

;; (pretty-display (syntax->datum
;;                  (expand-once (expand-once #'(get-attributes current-project current-leaf point)))))


;; (pretty-display (syntax->datum (expand-once #'(produce-epc-methods
;;                                                 #:on-object current-project
;;                                                 #:for-side-effects? #t
;;                                                 #:methods select-next))))



;; (define-syntax (make-attribute-getter stx)
;;   (syntax-parse stx
;;     [(_ object:id)
;;      (let ((splitted-ids (string-split (symbol->string (syntax-e #'object)))))]))

(define-syntax (produce-epc-methods stx)
  
  (syntax-parse stx
    [(_ #:on-object object:expr
        #:prefix prefix
        #:methods method:id ...)
     (with-syntax ([(method-name ...) (stx-map (curryr prefix-id #'prefix) #'(method ...))])
                   
       #'(begin (define-epc-method (method-name . args)
                  (send object method . args))
                ...))]))




;; (define-epc-methods
;;   #:on-object current-project
;;   #:for-side-effects #t
;;   select-next
