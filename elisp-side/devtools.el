;;; -*- lexical-binding: t -*-

(require 'ss-rpc-client)
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



(defvar ss:racket-exec-path
  ;; "~/local/racket/bin/racket"
  "racket"
  "path to racket executable")

(setq max-lisp-eval-depth 10000)

(defun ss:start-racket-server (name server-path)
  ;; server shouldn't produce any output to stdout/err
  (ss:start-server name (format "%s %s" ss:racket-exec-path server-path)))

  ;; (ss:start-server name (format "export PLTSTDERR=\"error none@ss-rpc\"; %s %s 2>ss-rpc-err.txt"
  ;;       			  ss:racket-exec-path
  ;;       			  server-path)))


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
		  "/home/kotik/Projects/devtools-stable/ss-rpc.log")
    (switch-to-buffer log-buffer)))


(defvar dt:server
  (ss:start-racket-server "devtools" (format "~/Projects/%s/server.rkt" devtools-dir)))

(defun dt:call (proc &rest args)
  (apply #'ss:call dt:server proc args))

(defun dt:call! (proc &rest args)
  (apply #'ss:call! dt:server proc args))


(provide 'devtools)

