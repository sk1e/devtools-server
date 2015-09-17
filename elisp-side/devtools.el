;;; -*- lexical-binding: t -*-

(require 'serp)
(require 'dt-buffer)
;; (require 'dt-server)
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
		  "/home/god/Projects/devtools/serp.log"
		  ;"/home/god/serp.log"
		  ;(format  "~/Projects/%s/serp.log" devtools-dir)
		  )
    (switch-to-buffer log-buffer)))
  ;; (call-process "grep" nil "bar" nil "lewis" "/etc/passwd")
  ;; ())

;; (file-exists-p "~/Projects/devtools/serp.log")

;; '(:weight ultra-bold
;; 	  :height 110
;; 	  :family "Liberation Mono"
;; 	  :foreground "CornflowerBlue")



(defvar dt:server
  (serp:start-racket-server "devtools" (format "~/Projects/%s/server.rkt" devtools-dir)))

(defun dt:call (proc &rest args)
  (apply #'serp:call dt:server proc args))

(defun dt:call/call-return (proc &rest args)
  (apply #'serp:call/call-return dt:server proc args))

(dt:call/call-return 'init-indicator-symbols!)

(provide 'devtools)


;; (sp:client-procedures
;;  (defun sp:yes-or-no-p (prompt)
;;    (if (yes-or-no-p prompt)
;;        '\#t
;;      '\#f)))

;; (setq sp:client-proc-list
;;       (append sp:client-proc-list
;; 	      '(yes-or-no-p read-string)))

;; (sp:server-start)

;;(insert (propertize "q" 'display '((margin right-margin) "asd")))

;; (cl-prettyprint (macroexpand '(call-sg-procedure qwe ())))

;; (cl-prettyprint (macroexpand '(define-remote/interactive/void-procedures (qwe))))

;; (goto-char (1+ (point-min)))
;; (set-left-margin 0 10 10)

;; (set-window-margins (selected-window) 0 4)

;; (insert (propertize "w" 'display '((margin right-margin) "lol")))
;; (insert (propertize "q" 'display (list '(margin right-margin) tt)))
;; ;;(insert (propertize "q" 'display '(image ((:type png :file "/home/god/pic.png")))))
;; (insert (propertize "q" 'display tt))

;; (insert (propertize "w" 'display `((margin right-margin) ,tt)))

;; (file-exists-p "~/Projects/devtools/tree/indicator-pics/modified-DarkRed.png")
;; (defimage ttt
;;   ((:type png :file "~/Projects/devtools/tree/indicator-pics/modified-DarkRed.png" :ascent 90)))


;; (insert (prin1-to-string ttt))
;; (insert (propertize "q" 'display '((margin right-margin) (image :type png :file "/home/god/Projects/devtools/tree/indicator-pics/modified-DarkRed.png" :ascent 90))))

;; (insert (propertize  "q" 'display `((margin right-margin) ,ttt)))
;; (insert (propertize  "q" 'display ())))

;; (insert )

;; (defimage qwe
;;   ((:type png :file "/home/god/Projects/srcgraph/pics/modified-BlueViolet.png" :ascent 90)))

;; (defimage asdf
;;   ((:type png  :file "/home/god/Projects/srcgraph/tree/indicator-pics/modified-DarkRed.png" :ascent 90)))

;; (defimage indicator-modified-DarkRed
;;   ((:type png  :file "/home/god/Projects/srcgraph/tree/indicator-pics/modified-DarkRed.png" :ascent 90)))


;; (insert (propertize "q" 'display '((margin right-margin) "asd")))

;; '(image (:type png :file "~/pic.png"))

;; (right-margin-width)

