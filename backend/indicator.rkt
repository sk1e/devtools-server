#lang racket/base


(require racket/draw
         racket/string
         racket/runtime-path
         racket/contract
         
         ss/racket/class

         "buffer.rkt"
         ;"../tree/ebuffer.rkt"
         "../sg-epc.rkt")

(provide indicator<%>
         modified-indicator%
         test-indicator%)


(define-runtime-path self-path "indicator.rkt")
(define-values (self-parent-path _ __) (split-path self-path))
(define pic-directory-path (build-path self-parent-path "indicator-pics"))
;(define blank-path (build-path pic-directory-path "bl"))

;; (define colors '("DarkRed" "Orange" "BlueViolet"))



(define indicator<%>
  (interface ()

    [init-color (->i ([this any/c]
                      [color (this) (λ (v) (member v (send this possible-colors)))])
                     [res any/c])]
    [init-node (->m ebuffer:descendant<%> void?)]
    [init-position (->m natural-number/c)]
    
    [render (->m (is-a?/c bitmap-dc%) string? void?)]
    [produce-pictures (->m void)]
    
    [name (->m string?)]
    [possible-colors (->m (listof string?))]
    
    [picture-path (->m string? path-string?)]
    [picture-symbol (->m string? symbol?)]

    [representation (->m buffer-string?)]
    
    ))






(define abstract-indicator%
  (class* object% (indicator<%>)
    (super-new)
    
    (abstract render
              name
              possible-colors)

    
    (field [color 'uninitialized]
           [node 'uninitialized]
           [position 'uninitialized]
           [show? #f])
    
    
    (define/public (init-color v) (set! color v))
    (define/public (init-node v) (set! node v))
    (define/public (init-position v) (set! position v))

    
    (define/public (representation)
      (make-buffer-string (" " 'display (cond
                                         [show? `((margin right-margin) ,(picture-symbol))]
                                         [else ""]))))
    
    (define/public (toggle-showing)
      (set! show? (not show?))
      (define point (buffer-point))
      (send+ node
             (tree-buffer)
             (put-text-property point (add1 point) 'display (cond
                                                             [show? `((margin right-margin) ,(picture-symbol))]
                                                             [else " "]))))
      
      



    (define/public (buffer-point)
      (+ position (send node resultant-point)))

    (define/public (picture-path)
      (cond
       [show? (path-for-color color)]
       [else (build-path pic-directory-path)]))
    
    (define/public (produce-pictures)
      (for ([color (possible-colors)])
        
        (define target (make-bitmap 15 15))
        (define dc (new bitmap-dc% [bitmap target]))
        (render dc color)

        (send target save-file (picture-path color) 'png)))

    
    (define/public (path-for-color color)
      (build-path pic-directory-path (format "~a-~a.png" (name) color)))
    
    (define/public (picture-symbol)
      (string->symbol (format "indicator-~a-~a" (name) color)))

        
    ))

(define modified-indicator%
  (class abstract-indicator%
    (super-new)

    (define/override (render dc color)
      (send* dc
        (set-pen color 2 'solid)
        (draw-line 2 2 2 13)
        (draw-line 5 2 9 13)))


    (define/override (name) "modified")
    (define/override (possible-colors) '("DarkRed"))

    
    ))


(define test-indicator%
  (class abstract-indicator%
    (super-new)


    (define/override (render dc color)
      (send* dc
        (set-pen color 1 'solid)
        (set-brush "red" 'transparent)
        (draw-rectangle 2 6 8 8)
                
        (draw-line 2 6 5 2)
        (draw-line 5 2 13 2)
        (draw-line 10 6 13 2)
        (draw-line 13 2 13 9)
        (draw-line 9 13 13 9)
        
        
        ;; (draw-line 5 2 9 13)
        
        ))

    (define/override (name) "test")
    (define/override (possible-colors) '("BlueViolet"))
    
    ))





(define (make-pics)
  (unless (directory-exists? pic-directory-path)
    (make-directory pic-directory-path))
  (for-each (compose (method produce-pictures) make-object)
            (list modified-indicator% test-indicator%)))


;(make test-indicator% [color "BlueViole"])

;(make-pics)


