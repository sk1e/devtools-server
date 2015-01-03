#lang racket/base

(require racket/contract
         racket/match
         racket/function
         racket/vector
         racket/format
         racket/list
         racket/set
         racket/string
         
         ss/racket/class
         ss/racket/syntax

         "../sg-epc.rkt"
         ;"specifier.rkt"

         (for-syntax racket/base
                     
                     syntax/parse
                     syntax/stx))

(provide buffer-string%
         buffer-string?
         buffer-string-chunk%

         buffer<%>
         emulated-buffer%
         emacs-buffer%

         chunk-binary-search
         
         make-buffer-string
         ;; property-equal?
         property-update)


(define-logger buffer)


(define (buffer-string? v) (is-a? v buffer-string<%>))

(define (buffer-string-chunk? v) (is-a? v buffer-string-chunk<%>))

;; (define/contract (property-equal? prop1 prop2)
;;   (-> hash? hash? boolean?)
;;   (and (set=? (list->set (hash-keys prop1))
;;               (list->set (hash-keys prop2)))
;;        (let equal-values? ([keys (hash-keys prop1)])
;;          (match keys
;;            [(cons x xs) (and (equal? (hash-ref prop1 x)
;;                                      (hash-ref prop2 x))
;;                              (equal-values? xs))]
;;            ['() #t]))))

(define/contract (property-update source-prop updating-prop)
  (-> hash? hash? hash?)
  (let update ([keys (hash-keys updating-prop)])
    (match keys
      [(cons x xs) (hash-set (update xs) x (hash-ref updating-prop x))]
      ['() source-prop])))


(define buffer-string-chunk<%>
  (interface ()
    [substring-chunk (->*m (natural-number/c) (natural-number/c) buffer-string-chunk?)]
    [shift (->m integer? buffer-string-chunk?)]
    [propertize (->m hash? buffer-string-chunk?)]
    [init-value! (->m string? void?)]
    [init-properties! (->m hash? void?)]
    [init-end-point! (->m natural-number/c void?)]
    [property-list (->m list?)]
    ))

(define buffer-string-chunk%
  (class* object% (buffer-string-chunk<%> equal<%> printable<%>)
    (super-new)

    (field [value 'uninitialized]
           [properties #hash()]
           [end-point 'uninitialized])

    (define/public (property-list) (apply append (map (λ (p) (cons (car p) (cons (cdr p) '())))
                                                      (hash->list properties))))

    (define/public (start-point) (- end-point (string-length value)))
    
    
    (define/public (equal-to? other recur)
      (unless (and (equal? value (get-field value other))
                   (equal? properties (get-field properties other))
                   (equal? end-point (get-field end-point other)))
        
        (printf ">>>>>>>>>>>>>>\n~v ~v ~v\n~v ~v ~v\n"
                value properties end-point
                (get-field value other) (get-field properties other) (get-field end-point other)))
      
      (and (equal? value (get-field value other))
           (equal? properties (get-field properties other))
           (equal? end-point (get-field end-point other))))
    
    
    (define/public (equal-hash-code-of hash-code)           (error 'not-implemented))
    (define/public (equal-secondary-hash-code-of hash-code) (error 'not-implemented))

    (define (repr-props)
      (string-join (map
                    (λ (pair) (format "#:~a ~a" (car pair) (cdr pair)))
                    (hash->list properties))))

    
    (define (repr) (format "buffer-string-chunk<%>:\nvalue: ~a\nprops: ~a\nend-point: ~a\n----\n" (~s value) (~s (repr-props)) end-point))

    (define/public (custom-write out)       (write (repr) out))
    (define/public (custom-display out)     (display (repr) out))
    (define/public (custom-print out depth) (display (repr) out))

    
    (define/public (init-value! v) (set! value v))
    (define/public (init-properties! v) (set! properties v))
    (define/public (init-end-point! v) (set! end-point v))


    (define/public (substring-chunk from [to end-point])
      (define offset (start-point))
      (define substring-value (substring value
                                         (- from offset)
                                         (- to offset)))
      (make buffer-string-chunk%
        [value (substring value
                          (- from offset)
                          (- to offset))]
        [end-point 0]
        [properties properties]))

    

    (define/public (propertize prop-list)
      (make buffer-string-chunk%
        [value value]
        [properties (property-update properties prop-list)]
        [end-point end-point]))

    (define/public (shift offset)
      (make buffer-string-chunk%
        [value value]
        [properties properties]
        [end-point (+ end-point offset)]))

    ))

;; (define lst '(a b c d e f))
;; (for/list ([e lst]
;; 	   [n (range (length lst))]
;; 	   #:when (even? n))
;; e)


(define/contract (chunk-binary-search vec point)
  (-> vector? natural-number/c natural-number/c)
  (cond
   [(eq? (vector-length vec) 1) 0]
   [else (let search ([left 0]
                      [right (vector-length vec)])
           
           (define idx (quotient (+ left right) 2))
           (define elt (vector-ref vec idx))
           (define elt-point (get-field end-point elt))
           
           (cond
            [(< right left) left]
            [(< point elt-point) (search left (sub1 idx))]
            [(>= point elt-point) (search (add1 idx) right)]))]))


(define buffer-string<%>
  (interface ()
    [concat (->*m () #:rest (listof buffer-string?) buffer-string?)]
    [bs-substring (->*m (natural-number/c) (natural-number/c) buffer-string?)]
    [bs-length (->m natural-number/c)]
    [bs-no-properties (->m string?)]
    [propertize (->m hash? buffer-string?)]
    ))




(define buffer-string%
  (class* object% (buffer-string<%> equal<%> printable<%>)
    (super-new)
    
    (field [chunk-vector #()])

    (define/public (equal-to? other recur)
      (equal? chunk-vector (get-field chunk-vector other)))
    
    (define/public (equal-hash-code-of hash-code)           (error 'not-implemented))
    (define/public (equal-secondary-hash-code-of hash-code) (error 'not-implemented))
    
    (define (repr)  (format "buffer-string<%>:\nchunk-vector: ~a\n----\n" chunk-vector))
    
    (define/public (custom-write out)       (write (repr) out))
    (define/public (custom-display out)     (display (repr) out))
    (define/public (custom-print out depth) (display (repr) out))

    

    (define/public (init-chunk-vector! v) (set! chunk-vector v))
    
    
    (define/public (init-string! str [properties #hash()])
      (set! chunk-vector (vector (make buffer-string-chunk%
                                   [value str]
                                   [properties properties]
                                   [end-point (string-length str)]))))

    (define/public (propertize props)
      (make buffer-string% [chunk-vector (vector-map (curryr (method propertize) props)
                                                     chunk-vector)]))
    

    (define/public (bs-no-properties)
      (apply string-append (map (field-getter value) (vector->list chunk-vector))))

    (define/public (bs-length)
      (define len (vector-length chunk-vector))
      (cond
       [(zero? len) 0]
       [else (get-field end-point (vector-ref chunk-vector (sub1 len)))]))
    
    (define filter-empty (curry vector-filter (λ (e) (not (equal? (get-field value e) "")))))
    
    (define/public (concat-one str)
      (make buffer-string% (chunk-vector (vector-append (filter-empty chunk-vector)
                                                        (vector-map (curryr (method shift) (bs-length))
                                                                    (filter-empty (get-field chunk-vector str)))))))
    (define/public (concat . strings)
      (foldl (λ (x res) (send res concat-one x))
             this
             strings))

    
    (define/public (bs-substring from [to (bs-length)])
      (define (substring-from-one-chunk idx)
        (make buffer-string%
          [chunk-vector (vector (send+ (vector-ref chunk-vector idx)
                                       (substring-chunk from to)
                                       (shift (- to from))))]))

      
      
      (match (vector-length chunk-vector)
        [0 (match* (from to)
                   [(0 0) this]
                   [(_ _) (error 'bad-substring-range)])]
        [1 (substring-from-one-chunk 0)]
        [_  (define last-idx (sub1 (vector-length chunk-vector)))
            
            (define from-idx (cond
                              [(eq? from (bs-length)) last-idx]
                              [else (chunk-binary-search chunk-vector from)]))
            (define to-idx (cond
                            [(eq? to (bs-length)) last-idx]
                            [else (chunk-binary-search chunk-vector to)]))
           (cond
            [(eq? from-idx to-idx) (substring-from-one-chunk from-idx)]
            [else (let* ([first-chunk (vector-ref chunk-vector from-idx)]
                         [first-substringed-chunk (send+ first-chunk
                                                         (substring-chunk from)
                                                         (shift (get-field end-point first-chunk)))]
                         
                         [last-chunk (vector-ref chunk-vector to-idx)]
                         [last-substringed-chunk (send+ last-chunk
                                                        (substring-chunk (send last-chunk start-point) to)
                                                        (shift to))])

                    
                    (make buffer-string%
                      [chunk-vector (vector-map (curryr (method shift) (- from))
                                                (filter-empty (vector-append (vector first-substringed-chunk)
                                                                             (vector-copy chunk-vector (add1 from-idx) to-idx)
                                                                             (vector last-substringed-chunk))))]))])]))
    
  

    ))





;; (define-syntax (make-buffer-string stx)
;;   (define (make-constructor string-stx)
;;     (syntax-parse string-stx
;;       [(x:expr prop:expr ...)  #'(let ([str (new buffer-string%)])
;;                                    (send str init-string! x (list prop ...))
;;                                    str)]
;;       [x:id #'x]
      
;;       [x:expr #'(let ([str (new buffer-string%)])
;;                   (send str init-string! x)
;;                   str)]))
;;   (syntax-parse stx
;;     [(_ x xs ...)
;;      (with-syntax ([x-constructor (make-constructor #'x)]
;;                    [(xs-constructor ...) (stx-map make-constructor #'(xs ...))])
       
;;        #'(send x-constructor concat xs-constructor ...))]))

(define-syntax (make-buffer-string stx)
  (define (make-constructor string-stx)
    (syntax-parse string-stx
      [(x:expr (~seq prop:expr value:expr) ...)
       #'(let ([str (new buffer-string%)])
           (send str init-string! x (make-immutable-hash (map cons (list prop ...) (list value ...))))
           str)]
      
      [x:id #'x]
      
      [x:expr #'(let ([str (new buffer-string%)])
                  (send str init-string! x)
                  str)]))
  
  (syntax-parse stx
    [(_ x xs ...)
     (with-syntax ([x-constructor (make-constructor #'x)]
                   [(xs-constructor ...) (stx-map make-constructor #'(xs ...))])
       
       #'(send x-constructor concat xs-constructor ...))]))




(define buffer<%>
  (interface ()
    [substring-content (->m natural-number/c natural-number/c buffer-string?)]
    
    [insert (->*m ((is-a?/c buffer-string%))
                  (#:point (or/c natural-number/c #f))
                  void?)]
    [delete-region (->m natural-number/c natural-number/c void?)]
    [put-text-property (->m natural-number/c natural-number/c symbol? any/c void?)]
    [put-text-property/eval (->m natural-number/c natural-number/c symbol? any/c void?)]
    [set-header! (->m buffer-string? void?)]
    [goto-char! (->m natural-number/c void?)]
    [clear-buffer! (->m void?)]
    [switch-to-buffer! (->m void)]
    [revert! (->m void?)]
    [kill! (->m void?)]
    call
    
    ))


;; todo make serializable
(define abstract-buffer%
  (class* object% (buffer<%>)
    (super-new)
    (init-field name)

    (abstract insert
              delete-region
              call
              substring-content
              put-text-property
              put-text-property/eval
              goto-char!
              clear-buffer!
              set-header!
              switch-to-buffer!
              revert!
              kill!)
    ))


(define emulated-buffer%
  (class abstract-buffer%
    (super-new)
    
    (field [point 1]
           [content (new buffer-string%)]
           [header #f])

    (define/override (clear-buffer!)
      (set! point 1)
      (set! content (new buffer-string%)))
    
    (define/override (insert buffer-string  #:point [pnt point])
      ;; (printf "insert: ~v\n" (send buffer-string bs-no-properties))
      (let ([pnt (sub1 pnt)])
        (with-method ([substring-content (content bs-substring)])
          (set! content (send (substring-content 0 pnt)
                              concat
                              buffer-string
                              (substring-content pnt)))
          ))


      (set! point (+ pnt (send buffer-string bs-length))))
    

    (define/override (call proc . args)
      (printf "\nWARNING, registered attempt to call unimplemented buffer procedure `~a'\n" proc))
    
    (define/override (delete-region from to)
      ;; (printf "from ~a to ~a\n" from to)
      ;; (printf "content:\n~a\n" (send content bs-no-properties))
      (set! content (send (substring-content 1 from)
                          concat
                          (substring-content to)))
      ;; (printf "after:\n~a\n" (send content bs-no-properties))
      (set! point (- point (- to from))))

    (define/override (put-text-property from to prop value)
      (set! from (sub1 from))
      (set! to (sub1 to))
      (with-method ([substring-content (content bs-substring)])

        (set! content
              (send (substring-content 0 from)
                    concat
                    (make buffer-string%
                      [chunk-vector (vector-map (curryr (method propertize) (make-immutable-hash (list (cons prop value))))
                                                (get-field chunk-vector (substring-content from to)))])
                    (substring-content to)))))

    (define/override (put-text-property/eval from to prop value)
      (put-text-property from to prop value))


    (define/override (substring-content from [to (add1 (send content bs-length))])
      (send content bs-substring (sub1 from) (sub1 to)))

    (define/override (goto-char! pos) (void)
      ;(set! point (sub1 pos))
      )
    

    (define/override (set-header! v) (set! header v))

    (define/override (switch-to-buffer!) (void))
    (define/override (revert!) (void))
    (define/override (kill!) (set! content 'killed))
    
    ))

;(vector-binary-search )


(define emacs-buffer%
  (class abstract-buffer%
    (super-new)

    (inherit-field name)
    (define/override (call proc . args)
      ;;(log-buffer-debug "call ~v" `(lambda () (with-current-buffer ,name (apply ',proc `args))))
      
      (el-deferred-call 'call-in-buffer name proc args)
      ;; (el-call `(lambda () (with-current-buffer ,name ,(cons proc args) ;(,proc ,@args)
      ;;                                           ;;(apply ',proc (list ,@args))
      ;;                                           )))
      )
    
    
    (define/override (insert buffer-string #:point [point #f])
      (when point
        (el-deferred-call `(lambda () (with-current-buffer ,name (goto-char ,point)))))

      
      (for ([chunk (get-field chunk-vector buffer-string)])
        (el-deferred-call 'insert-to-propertized (get-field value chunk) name (send chunk property-list))))

    (define/override (set-header! buffer-string)
      (el-deferred-call 'call-in-buffer name '(lambda () (setq header-line-format nil)))
      
      (for ([chunk (get-field chunk-vector buffer-string)])
        (el-deferred-call 'append-to-header-propertized (get-field value chunk) name (send chunk property-list))))

    
    (define/override (delete-region from to)
      (call 'delete-region from to))

    (define/override (put-text-property from to face value)
      (call 'put-text-property from to face value))

    (define/override (put-text-property/eval from to face value)
      (call 'put-text-property/eval from to face value))
    
    (define/override (substring-content from to)
      (error 'not-implemented))

    (define/override (goto-char! pos)
      (call `(lambda () (set-window-point (get-buffer-window ,name) ,pos))))

    (define/override (clear-buffer!)
      (call `(lambda () (delete-region 1 (point-max)))))
    
    (define/override (switch-to-buffer!) (el-deferred-call 'switch-to-buffer name))
    (define/override (revert!) (call 'revert-buffer 't 't 't))
    (define/override (kill!) (el-deferred-call 'kill-buffer name))
    
    ))







;; (define-syntax (define-buffer stx)
;;   (syntax-parse stx 
;;     [(_ buffer-id:id buffer-name:str)
;;      (with-syntax ([emulated-id (prefix-id #'buffer-id:id #'emulated-)]
;;                    [emacs-id    (prefix-id #'buffer-id:id #'emacs-]])
;;        #'(begin (define emulated-id (new emulated-buffer% [name buffer-name]))
;;                 (define emacs-id    (new emacs-buffer%    [name buffer-name]))))]))











