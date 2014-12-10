#lang racket/base

(require "backend/buffer.rkt")

(provide (prefix-out const: (all-defined-out)))


(define racket-path "/home/god/racket")

(define projects-path "/home/god/Projects")
(define project-cache-file-name "project-cache")


(define ebuffer-tree-indentation 4)

(define project-tree-buffer-name "ftree")

(define selection-background-color "gray19")
(define execution-foreground-color "DeepSkyBlue")
(define current-version-foreground-color "DeepSkyBlue")


(define buffer-base-path-face '(:foreground "dim gray"))
(define buffer-name-path-face '(:foreground "white"))

(define indicator-modified-warning-color "DarkRed")
(define indicator-test-warning-color "DarkRed")
(define indicator-test-base-color "DarkGray")


;(define-values ())


(define project-tree-buffer (make-buffer "ftree"))
(define git-tree-buffer (make-buffer "git-tree"))


;(define-buffer project-tree-buffer "ftree")


