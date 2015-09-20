;;; -*- lexical-binding: t -*-

(require 'serp)
(require 'dt-buffer)
(require 'dt-project)
;; (require 'dt-git)


(defface dt:ebuffer-node-face 
  '((t :family "Liberation Mono")) "")

(defface dt:root-face
  '(( t :inherit dt:header-face :foreground "gray68")) "")


(defface dt:header-face 
  '((t :inherit dt:ebuffer-node-face
       :height 110
       :weight ultra-bold)) "")



(defvar serp:racket-exec-path
  "~/racket/bin/racket"
  "path to racket executable")

(defun serp:start-racket-server (name server-path)
  ;; server shouldn't produce any output to stdout/err
  (serp:start-server name (format "export PLTSTDERR=\"error none@serp\"; %s %s 2>serp-err.txt"
				  serp:racket-exec-path
				  server-path)))


(defun kill-nahuy ()
  (interactive)
  (setq kill-emacs-hook nil)
  (kill-emacs))


(defun dt:show-server-log ()
  (interactive)
  (let ((log-buffer (get-buffer-create "devtools server log")))
    (with-current-buffer log-buffer
      (setq buffer-read-only nil)
      (delete-region 1 (point-max))
      (setq buffer-read-only t))
    (call-process "tail" nil log-buffer nil "-n 100"
		  "/home/god/Projects/devtools-stable/serp.log")
    (switch-to-buffer log-buffer)))


(defvar dt:server
  (serp:start-racket-server "devtools" (format "~/Projects/%s/server.rkt" devtools-dir)))

(defun dt:call (proc &rest args)
  (apply #'serp:call dt:server proc args))

(defun dt:call/call-return (proc &rest args)
  (apply #'serp:call/call-return dt:server proc args))


(provide 'devtools)

