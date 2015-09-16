#lang racket/base

(require racket/contract
         racket/match
         racket/function
         racket/format
         racket/draw
         racket/runtime-path
         
         ss/racket/class
         ss/racket/provide

         serp

         "../backend/buffer.rkt"
         "../backend/buffer-string.rkt"
         "../backend/emacs.rkt"
         
         "../constants.rkt"

         "base.rkt"
         "segment.rkt"
         "serialization.rkt"
         )



(provide (prefix-out ebuffer: (combine-out (suffixed-as interface mixin class
                                                        #:from (all-defined-out))
                                           current-node?
                                           indicator?)))

(define-namespace-anchor anchor)
;; (define ns (namespace-anchor->namespace anchor))
;; (eval '(new object%)
;;       ns)



(define-logger ebuffer)

(define node<%>
  (interface (segment:node<%>)
    
    [get-name (->m string?)]    
    
    [tree-buffer (->m (is-a?/c buffer<%>))]
    [init-insert! (->m void?)]
    ))



(define node-mixin
  (mixin (segment:node<%>) (node<%>)
    (super-new)

    (abstract get-name
              tree-buffer
              init-insert!)


    ))



(define (descendant? v) (is-a? v descendant<%>))

(define (indicator? v) (is-a? v indicator<%>))

(define (current-node? obj)
  (eq? obj (get-field current-node (send obj root))))


(define descendant<%>
  (interface (node<%> segment:descendant<%>)
    [default-face (->m symbol?)]    
    
    [post-select! (->m void?)]
    [initialize-ebuffer-node! (->m void?)]
    
    [indicator-list (->m (listof indicator?))]
    [init-indicator-list! (->m void?)]
    [solo-representation-length (->m natural-number/c)]
    [solo-representation (->m buffer-string?)]
    [subtree-representation (->m string?)]
    
    [resultant-solo-end-point (->m natural-number/c)]
    [solo-end-point (->m natural-number/c)]
    
    [solo-insert! (->m void?)]
    [subtree-insert! (->m void?)]
    [insert/shift-solo! (->m void?)]
    [insert/shift-subtree! (->m void?)]
    
    [clear-solo! (->m void?)]
    [clear-subtree! (->m void?)]

    [remove-from-tree! (->m void?)]

    [put-extra-ftf-prop! (->m symbol? any/c void?)]
    [remove-extra-ftf-prop! (->m symbol? void?)]
    
    [fontify! (->m symbol? void?)]
    [mark-as-selected! (->m void?)]
    [mark-as-not-selected! (->m void?)]

    [select! (->m void?)]
    [select-as-new! (->m void?)]
    
    [select-next-leaf! (->m void?)]
    [select-prev-leaf! (->m void?)]
    [select-next-leaf-4! (->m void?)]
    [select-prev-leaf-4! (->m void?)]

    
    [select-next-intr! (->m void?)]
    [select-prev-intr! (->m void?)]
    ))

(define descendant-mixin
  (mixin (node<%> segment:descendant<%> serialization:descendant<%>) (descendant<%>)
    (super-new)

    (inherit-field point
                   parent)

    (field [indicators '()]
           [extra-ftf-prop-ht #hash()])
    
    (inherit get-name
             depth
             tree-buffer
             resultant-point
             resultant-offset
             root
             subtree-end-point
             shift-lower-nodes!
             remove!
             circular-next-leaf
             circular-prev-leaf
             circular-next-intr
             circular-prev-intr
             prev-leaf
             last-descendant-or-self
             infer-point!
             node-identifier)
    
    (abstract subtree-representation
              subtree-insert!
              default-face)

    (define/override (serialization-nodes)
      (append indicators (super serialization-nodes)))

    (define/override (field-setter-exprs)
      (cons `(set-field! indicators ,(node-identifier)
                         (list ,@(map (method node-identifier) indicators)))
            (super field-setter-exprs)))

    (define (indent-len) (* const:ebuffer-tree-indentation (sub1 (depth))))

    (define/public (init-indicator-list!)
      (set! indicators (indicator-list)))

    (define/public (indicator-list) '())
    
    (define/public (initialize-ebuffer-node!)
      (init-indicator-list!)
      (infer-point!)
      (insert/shift-subtree!))
    
    (define/public (switch-off-indicator! pos)
      (send (list-ref indicators pos) switch-off!))

    (define/public (switch-on-indicator! pos)
      (send (list-ref indicators pos) switch-on!))
    
    
    (define/public (solo-representation)
      (bs-append (apply bs-append (map (method representation) indicators))
                 (make-buffer-string ((string-append (make-string (indent-len) #\space) (get-name) "\n")
                                      'font-lock-face (default+extra-face)))))


    (define (default+extra-face)
      (match extra-ftf-prop-ht
        [(hash-table) (default-face)]
        [(hash-table (key value) ...) `(:inherit ,(default-face)
                                                 ,@(apply append (map list key value)))]))

    (define (refontify-with-extra!)
      (fontify! (default+extra-face)))
      
            
    (define/public (solo-representation-length)
      (+ (indent-len) (string-length (get-name)) (length indicators) 1))


    
    (define/public (solo-end-point) (+ point (solo-representation-length)))
    
    (define/public (resultant-subtree-end-point) (+ (resultant-offset) (subtree-end-point)))
    
    (define/public (resultant-solo-end-point) (+ (resultant-point) (solo-representation-length)))
    

    
    (define/override (init-insert!)
      (send (tree-buffer) insert (solo-representation)))
    
    (define/public (fontify! face)
      (send (tree-buffer)
            put-text-property
            (+ (resultant-point) (length (indicator-list)))
            (resultant-solo-end-point)
            'font-lock-face
            face))

    (define/public (put-extra-ftf-prop! key value)
      (set! extra-ftf-prop-ht (hash-set extra-ftf-prop-ht key value))
      (refontify-with-extra!))

    (define/public (remove-extra-ftf-prop! key)
      (set! extra-ftf-prop-ht (hash-remove extra-ftf-prop-ht key))
      (refontify-with-extra!))


    (define/public (mark-as-selected!)
      (put-extra-ftf-prop! ':background const:selection-background-color))


    (define/public (mark-as-not-selected!)
      (remove-extra-ftf-prop! ':background))
        
    (define/public (select!)
      (mark-as-selected!)
      (set-field! current-node (root) this)
      (post-select!)
      (send (tree-buffer) goto-char! (resultant-point)))

    (define/public (post-select!) (void))

    (define (current-node) (get-field current-node (root)))
    
    (define/public (select-as-new!)
      (send (current-node) mark-as-not-selected!)
      (select!))
    
    
    (define/public (select-next-leaf!)
      (cond
       [(circular-next-leaf) => (method select-as-new!)]))
    
    (define/public (select-prev-leaf!)
      (cond
       [(circular-prev-leaf) => (method select-as-new!)]))


    
    (define/public (select-next-intr!)
      (cond
       [(circular-next-intr) => (method select-as-new!)]))

    (define/public (select-prev-intr!)
      (cond
       [(circular-prev-intr) => (method select-as-new!)]))

    
    
    
    (define (select-nth! nav-proc repeats)
      (let loop ([n repeats]
                 [node (current-node)])
        (cond [(zero? n) (send node select-as-new!)]
              [else (loop (sub1 n) (nav-proc node))])))

    (define/public (select-next-leaf-4!)
      (when (send (root) has-leafs?)
        (select-nth! (method circular-next-leaf) 4)))
    
    (define/public (select-prev-leaf-4!)
      (when (send (root) has-leafs?)
        (select-nth! (method circular-prev-leaf) 4)))

    
    (define (clear! from to)
      (send (tree-buffer) delete-region from to)
      (shift-lower-nodes! (- from to)))
    
    (define/public (clear-subtree!)
      (clear! (resultant-point) (resultant-subtree-end-point)))
    

    (define/public (clear-solo!)
      (clear! (resultant-point) (resultant-solo-end-point)))


    (define/public (remove-from-tree!)      
      (clear-subtree!)
      (remove!))
    

        
    (define/public (solo-insert!)
      (send (tree-buffer) insert (solo-representation) #:point (resultant-point)))


    (define/public (insert/shift-subtree!)
      (subtree-insert!)
      (shift-lower-nodes! (- (resultant-subtree-end-point) (resultant-point))))


    (define/public (insert/shift-solo!)
      (solo-insert!)
      (shift-lower-nodes! (- (resultant-solo-end-point) (resultant-point))))
    

    (define/override (pre-lift! self prev)
      (for-each (method clear-subtree!) (list self prev)))

    
    (define/override (post-lift! self prev)
      (send* self (infer-point!) (insert/shift-subtree!))
      (send* prev (infer-point!) (insert/shift-subtree!)))
    
    
    ))


(define ancestor<%>
  (interface (node<%> segment:ancestor<%> serialization:ancestor<%>)
    ))


(define ancestor-mixin
  (mixin (node<%> segment:ancestor<%> serialization:ancestor<%>) (ancestor<%>)
    (super-new)


    ))




(define root<%>
  (interface (ancestor<%> segment:root<%> serialization:root<%>)
    [pre-tree-insert! (->m void?)]
    [init-tree-buffer! (->m void?)]
    [insert-tree! (->m void?)]
    [tree-representation (->m string?)]
    
    [lift-current-node! (->m void?)]
    [lower-current-node! (->m void?)]
    
    [select-by-name! (->m string? void?)]
    ))

(define root-mixin
  (mixin (ancestor<%> segment:root<%> serialization:root<%>) (root<%>)
    (super-new)
    
    (inherit descendants
             tree-buffer
             get-name
             node-identifier
             has-leafs?
             has-intrs?)
    
    (inherit-field children)

    (field [current-node #f])

    (inherit find-dfs)
    
    (define/override (field-setter-exprs)
      (cons `(set-field! current-node ,(node-identifier) ,(send current-node node-identifier))
            (super field-setter-exprs)))
    
    
    (define/public (tree-representation)
      (apply string-append (map (method subtree-representation) children)))


    (define/override (init-insert!) (void))

    (define/public (pre-tree-insert!)
      (send (tree-buffer) clear-buffer!))

    (define/public (insert-tree!)
      (pre-tree-insert!)
      (for-each (method init-insert!) (descendants)))

    (define/public (select-by-name! name)
      (send (find-dfs (λ (node) (equal? (send node get-name) name)))
            select-as-new!))
    
    (define/public (init-tree-buffer!)
      (pre-tree-insert!)
      
      (for ([desc (descendants)])
        (send* desc (init-indicator-list!) (init-insert!) (infer-point!))))
    
    
    ;; (define/override (face) 'sg-root-face-wtf)
    
      
    ;; remove
    (define/public (lift-current-node!)
      (send current-node lift-if-possible!))
    
    (define/public (lower-current-node!)
      (send current-node lower-if-possible!))

    
    
    ))


(define intr<%>
  (interface (descendant<%> ancestor<%> segment:intr<%> serialization:intr<%>)
    ))

(define intr-mixin
  (mixin (descendant<%> ancestor<%> segment:intr<%> serialization:intr<%>) (intr<%>)
    (super-new)

    (inherit solo-end-point
             solo-insert!
             solo-representation
             solo-representation-length
             last-descendant-or-self
             shift-children!
             resultant-point
             resultant-solo-end-point)
    
    (inherit-field children)

    (define/override (default-face) 'pt:intr-face)
    

    (define/override (subtree-end-point)
      (cond
       [(null? children) (solo-end-point)]

       [else (define last-descendant (last-descendant-or-self))
             (apply + 
                    (send last-descendant subtree-end-point)
                    (map (field-getter point)
                         (send last-descendant all-ancestors-till this)))]))


    (define/override (subtree-insert!)
      (solo-insert!)
      (for-each (method subtree-insert!) children))
    
    (define/override (point-for-first-child) (solo-representation-length))



    (define/override (subtree-representation)
      (apply string-append
             (get-field value (solo-representation))
             (map (method subtree-representation) children)))


    (define/override (clear-solo!)
      (super clear-solo!)
      (shift-children! (- (resultant-point) (resultant-solo-end-point))))

    
    (define/override (insert/shift-solo!)
      (super insert/shift-solo!)
      (shift-children! (- (resultant-solo-end-point) (resultant-point))))
    
    
    ))


(define leaf<%>
  (interface (descendant<%> serialization:leaf<%>)
    ))

(define leaf-mixin
  (mixin (descendant<%> serialization:leaf<%>) (leaf<%>)
    (super-new)
    
    (inherit solo-end-point
             solo-insert!
             solo-representation)
    
    (inherit-field parent)

    (define/override (default-face) 'pt:leaf-face)

    (define/override (subtree-end-point) (solo-end-point))

    (define/override (subtree-insert!) (solo-insert!))
    
    (define/override (subtree-representation)
      (get-field value (solo-representation)))

    
    ))




(define-composed-mixins
  [root-sum (node ancestor root)]
  [intr-sum (node descendant ancestor intr)]
  [leaf-sum (node descendant leaf)])

(define-composed-mixins
  [leaf-final-sum (segment:leaf-final-sum leaf-sum)]
  [intr-final-sum (segment:intr-final-sum intr-sum)]
  [root-final-sum (segment:root-final-sum root-sum)]
  [quasiroot-final-sum (segment:quasiroot-final-sum root-sum)])





(define-runtime-path self-path "ebuffer.rkt")
(define-values (self-parent-path _ __) (split-path self-path))
(define pic-directory-path (build-path self-parent-path "indicator-pics"))





(define indicator<%>
  (interface ()

    [init-color! (->i ([this any/c]
                      [color (this) (λ (v) (member v (send this possible-colors)))])
                     [res void?])]
    [init-node! (->m (is-a?/c descendant<%>) void?)]
    [init-position! (->m natural-number/c void?)]
    
    [render! (->m (is-a?/c bitmap-dc%) string? void?)]
    [produce-pictures! (->m void)]
    
    [name (->m string?)]
    [possible-colors (->m (listof string?))]
    
    [picture-path (->m string? path-string?)]
    [picture-symbol (->m string? symbol?)]

    [representation (->m buffer-string?)]

    [change-color! (->m string? void?)]
    
    ))




(define abstract-indicator%
  (class* (serialization:leaf-sum-mixin (base:leaf-sum-mixin object%))
    (indicator<%> serialization:node<%>); inspectable<%>)
    (super-new)

    (inherit node-identifier)
    
    (abstract render!
              name
              possible-colors)

    
    (field [color 'uninitialized]
           [node 'uninitialized]
           [position 'uninitialized]
           [show? #f])
    
    (define/override (field-setter-exprs)
      (cons `(send* ,(node-identifier)
               (init-color! ,color)
               (init-node! ,(send node node-identifier))
               [init-position! ,position]
               [init-show?! ,show?])
            (super field-setter-exprs)))
    
    (define/public (init-color! v)    (set! color v))
    (define/public (init-node! v)     (set! node v))
    (define/public (init-position! v) (set! position v))
    (define/public (init-show?! v)    (set! show? v))
    
    ;; (define (display-property) (cond
    ;;                             [show? `((margin right-margin) (eval ,(picture-symbol color)))]
    ;;                             [else  `((margin right-margin) (eval indicator-empty-Black))]))

    (define/public (image-list)
      `(image :type png :file ,(path->string (picture-path color)) :ascent 80))

    (define (display-property) (cond
                                [show? `((margin right-margin) ,(image-list))]
                                [else  `((margin right-margin) ,(send empty-indicator image-list))]))
    
    (define/public (representation)
      (make-buffer-string (" " 'display (display-property))))

    (define (refontify!)
      (define point (buffer-point))
      (send+ node
             (tree-buffer)
             (put-text-property/eval point (add1 point) 'display (display-property))))
    
    (define (switch! show-value)
      (set! show? show-value)
      (refontify!))


    

    (define/public (switch-on!)  (switch! #t))
    (define/public (switch-off!) (switch! #f))
    (define/public (toggle!)     (switch! (not show?)))

    (define/public (change-color! new-color)
      (set! color new-color)
      (refontify!))
    
    
    (define/public (buffer-point)
      (+ position (send node resultant-point)))


    
    (define/public (produce-pictures!)
      (for ([color (possible-colors)])
        
        (define target (make-bitmap 14 14))
        (define dc (new bitmap-dc% [bitmap target]))
        (render! dc color)

        (send target save-file (picture-path color) 'png)))

    
    (define/public (picture-path color)
      (build-path pic-directory-path (format "~a-~a.png" (name) color)))
    
    (define/public (picture-symbol color)
      (string->symbol (format "indicator-~a-~a" (name) color)))

    ;; (defimage tt
    ;;   ((:type png :file "~/Projects/srcgraph/pics/modified-BlueViolet.png" :ascent 90)))
    
    (define/public (produce-el-images)
      (for ([color (possible-colors)])
        (send (emacs) deferred-call `(lambda () (defimage ,(picture-symbol color)
                                                  ((:type png :file ,(path->string (picture-path color)) :ascent 80)))))))
    
    
    ))



(define indicator-list '())

(define-syntax-rule (define-indicator id body ...)
  (begin (define id body ...)
         (set! indicator-list (cons id indicator-list))))


(define-indicator empty-indicator%
  (class abstract-indicator%
    (super-new)

    (define/override (get-class-name) 'empty-indicator%)
    (define/override (render! dc color) (void))
    (define/override (name) "empty")
    (define/override (possible-colors) '("Black"))
    
    ))

(define empty-indicator (make empty-indicator% [color "Black"]))

(define-indicator modified-indicator%
  (class abstract-indicator%
    (super-new)

    (define/override (get-class-name) 'modified-indicator%)
    (define/override (render! dc color)
      (send* dc
        (set-pen color 2 'solid)
        (set-smoothing 'aligned)
        (draw-line 2 2 2 13)
        (draw-line 5 2 9 13)))


    (define/override (name) "modified")
    (define/override (possible-colors) (list const:indicator-modified-warning-color))
    
    
    ))


(define-indicator test-indicator%
  (class abstract-indicator%
    (super-new)

    (define/override (get-class-name) 'test-indicator%)
    (define/override (render! dc color)
      (send* dc
        (set-pen color 1 'solid)
        (set-brush "red" 'transparent)
        (set-smoothing 'aligned)

        (draw-line 0 0 0 5)
        (draw-line 0 0 5 0)

        (draw-line 8 0 13 0)
        (draw-line 13 0 13 5)

        (draw-line 0 8 0 13)
        (draw-line 0 13 5 13)

        (draw-line 8 13 13 13)
        (draw-line 13 8 13 13)

        (draw-line 2 6 5 6)
        (draw-line 2 7 5 7)

        (draw-line 8 6 11 6)
        (draw-line 8 7 11 7)))

    (define/override (name) "test")
    (define/override (possible-colors) (list const:indicator-test-base-color
                                             const:indicator-test-warning-color))
    
    ))







(define-serp-method (init-indicator-symbols!)
  (for-each (compose (method produce-el-images) make-object) indicator-list))

(define (make-pics)
  (unless (directory-exists? pic-directory-path)
    (make-directory pic-directory-path))
  (for-each (compose (method produce-pictures!) make-object) indicator-list))

(when (equal? (find-system-path 'run-file) self-path)
  (make-pics))

;; (list modified-indicator% test-indicator%)))

