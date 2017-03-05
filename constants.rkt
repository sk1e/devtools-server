 #lang racket/base

(require "backend/buffer.rkt")

(provide (prefix-out const: (all-defined-out)))


(define racket-path "/home/kotik/racket")

(define projects-path "/home/kotik/Projects")

(define project-data-directory-name ".project")
(define project-cache-file-name "cache.rkt")
(define project-config-file-name "config.rkt")

(define project-default-config #hash((ignored-files . #hash((names . ("compiled"
                                                                      "backup"
                                                                      "project-cache"
                                                                      "node_modules"
                                                                      "build"
                                                                      ".git"))
                                                            (regexps . ("[.]ttf$"
                                                                        "^.#"))))))

(define ebuffer-tree-indentation 3)


(define selection-background-color "gray19")
(define execution-foreground-color "DeepSkyBlue")
(define current-version-foreground-color "DeepSkyBlue")


(define buffer-base-path-face '(:foreground "dim gray"))
(define buffer-name-path-face '(:foreground "white"))

(define indicator-modified-warning-color "DarkRed")
(define indicator-test-warning-color "DarkRed")
(define indicator-test-base-color "DarkGray")


;(define-values ())
(define intr-stack-factor 3)

(define project-tree-buffer-name "project-tree")
(define git-tree-buffer-name "git-tree")

;; (define project-tree-buffer (make-buffer "ftree"))
;; (define git-tree-buffer (make-buffer "git-tree"))


;(define-buffer project-tree-buffer "ftree")


