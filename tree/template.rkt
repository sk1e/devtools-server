#lang racket/base

(require ss/racket/class
         ss/racket/provide)


(provide (prefix-out px: (suffixed-as interface mixin class
                                      #:from (all-defined-out))))




(define (node? v)       (is-a? v node<%>))
(define (descendant? v) (is-a? v descendant<%>))
(define (ancestor? v)   (is-a? v ancestor<%>))
(define (root? v)       (is-a? v root<%>))
(define (intr? v)       (is-a? v intr<%>))
(define (leaf? v)       (is-a? v leaf<%>))

(define node<%>
  (interface ()))


(define node-mixin
  (mixin () (node<%>)
    (super-new)))


(define descendant<%>
  (interface (node<%>)))

(define descendant-mixin
  (mixin (node<%>) (descendant<%>)
    (super-new)))



(define ancestor<%>
  (interface (node<%>)))

(define ancestor-mixin
  (mixin (node<%>) (ancestor<%>)
    (super-new)))



(define leaf<%>
  (interface (descendant<%>)))


(define leaf-mixin
  (mixin (descendant<%>) (leaf<%>)
    (super-new)))



(define intr<%>
  (interface (descendant<%> ancestor<%>)))

(define intr-mixin
  (mixin (descendant<%> ancestor<%>) (intr<%>)
    (super-new)))




(define root<%>
  (interface (ancestor<%>)))

(define root-mixin
  (mixin (ancestor<%>) (root<%>)
    (super-new)))



(define-composed-mixins
  [leaf-sum       (node descendant leaf)]
  [intr-sum       (node descendant ancestor intr)]
  [root-sum       (node ancestor root)])
