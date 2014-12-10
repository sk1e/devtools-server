;;; -*- lexical-binding: t -*-

(require 'sp-buffer)
(require 'sp-server)
(require 'sp-project)
(require 'sp-git)


(defface sp:ebuffer-node-face 
  '((t :family "Liberation Mono")) "")

(defface sp:root-face
  '(( t :inherit sp:header-face :foreground "gray68")) "")



;; '(:weight ultra-bold
;; 	  :height 110
;; 	  :family "Liberation Mono"
;; 	  :foreground "CornflowerBlue")



(defface sp:header-face 
  '((t :inherit sp:ebuffer-node-face
       :height 110
       :weight ultra-bold)) "")


(provide 'source-processor)


;; (sp:client-procedures
;;  (defun sp:yes-or-no-p (prompt)
;;    (if (yes-or-no-p prompt)
;;        '\#t
;;      '\#f)))

(setq sp:client-proc-list
      (append sp:client-proc-list
	      '(yes-or-no-p read-string)))

(sp:server-start)

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


;; (defimage tt
;;   ((:type png :file "~/Projects/srcgraph/pics/modified-BlueViolet.png" :ascent 90)))

;; (insert (propertize/eval  "q" 'display '((margin right-margin) (eval tt))))

;; (defimage qwe
;;   ((:type png :file "/home/god/Projects/srcgraph/pics/modified-BlueViolet.png" :ascent 90)))

;; (defimage asdf
;;   ((:type png  :file "/home/god/Projects/srcgraph/tree/indicator-pics/modified-DarkRed.png" :ascent 90)))

;; (defimage indicator-modified-DarkRed
;;   ((:type png  :file "/home/god/Projects/srcgraph/tree/indicator-pics/modified-DarkRed.png" :ascent 90)))


;; (insert (propertize "q" 'display '((margin right-margin) "asd")))

;; '(image (:type png :file "~/pic.png"))

;; (right-margin-width)

