;;; -*- lexical-binding: t -*-

(require 'epcs)

(setq epc:debug-out t)
;(setq lexical-binding t)

(defvar sg-epc nil)

(defvar sg-client-proc-list nil)

(defun sg-server-start ()
  (setq epc:accept-process-timeout 2000)
  (setq sg-epc (epc:start-epc "~/racket/bin/racket" '("~/Projects/devtools/remote-procedures.rkt")))
  (setq epc:accept-process-timeout 100)
  (sg-call-procedure/call-callback init-indicator-symbols '())
  
  ;; (loop for proc in sg-client-proc-list
  ;; 	do (epc:define-method sg-epc proc proc))
  )

;; (defun sg-server-start ()
;;   (deferred:$
;;     (deferred:next #'sg--server-start)))

;; (defmacro  define-remote/interactive/void-procedures (names)
;;   (loop for name in names
;; 	collect `(defun ,(intern (format "sg-%s" name)) ()
;; 		   (interactive)
;; 		   (sg-call-procedure ,name ()))
;; 	into defuns
;; 	finally return `(progn ,@defuns)))


(defmacro  define-remote/interactive/callback-calling-procedures (names)
  (loop for name in names
	collect `(defun ,name () ;;,(intern (format "sg-%s" name)) ()
		   (interactive)
		   (sg-call-procedure/call-callback ,name ()))
	into defuns
	finally return `(progn ,@defuns)))

(defmacro  remote-method (name)
  `(lambda () (sg-call-procedure/call-callback ,name nil)))

(defun insert-to (string buffer)
  (with-current-buffer buffer (insert string)))


(defun insert-to-propertized (string buffer properties)
  (with-current-buffer buffer (insert (apply #'propertize/eval string properties))))


(defun insert-to-at-point (string buffer point)
  (with-current-buffer buffer
    (goto-char point)
    (insert (string))))


(defun insert-to-propertized-at-point (string buffer properties point)
  (with-current-buffer buffer
    (goto-char point)
    (insert (apply #'propertize string properties))))



(defmacro  sg-call-procedure/call-callback (name args)
  ;; `(call-procedure-list (epc:call-sync sg-epc ',name ,args))
  
  `(deferred:$
     (epc:call-deferred sg-epc ',name ,args)
     
     (deferred:nextc it
       #'call-procedure-list)

     (deferred:error it
       (lambda (err) (message "%s" err))))

  
  )



(defun call-procedure-list (lst)
  (loop for (proc args) in lst
	;do (message ">> %s . %s" proc args)
	do (apply proc args)))


(defmacro  sg-client-procedures (&rest defuns)
  (loop for defun in defuns
	collect (second defun) into defun-names
	finally return `(progn (setq sg-client-proc-list ',defun-names)
			       ,@defuns)))

;; (cl-prettyprint (macroexpand '(sg-client-procedures
;; 			       (defun sg-fontify-region (buffer start-pos end-pos face)
;; 				 (with-current-buffer buffer
;; 				   (put-text-property start-pos end-pos 'font-lock-face face))))))



;; (defun el-fontify-region (buffer start-pos end-pos face)
;;   (with-current-buffer buffer
;;     (put-text-property start-pos end-pos 'font-lock-face face)))




(defun el-init-file-buffer (path new-name new-mode-line-buffer-identification-parts)
  (with-current-buffer (find-file-noselect path)
    (rename-buffer new-name)
    (setq mode-line-buffer-identification (cl-destructuring-bind ((base-string base-face)
								  (name-string name-face))
					      new-mode-line-buffer-identification-parts
					    (concat (propertize base-string 'font-lock-face base-face)
						    (propertize name-string 'font-lock-face name-face))))
    (set (make-local-variable 'project-buffer-modified-p) nil)
    
    (layout-mode)
    (sg-mode)))


(defun call-in-buffer (buffer proc args)
  (with-current-buffer buffer (apply proc args)))

(defun put-text-property/eval (start end property value)
  "`put-text-property' + eval support in property value"
  (put-text-property start end property (proc-property-value value)))


(defun propertize/eval (string &rest properties)
  "`propertize' + eval support in property value"
  (defun proc-properties (prop-pairs)
    (cond
     ((null prop-pairs) '())
     ((consp prop-pairs) (let ((f (first prop-pairs))
			       (s (second prop-pairs))
			       (xs (cddr prop-pairs)))
			   (cons f (cons (proc-property-value s)
					 (proc-properties xs)))))))
  (apply #'propertize string (proc-properties properties)))




(defun proc-property-value (value)
  (cond
   ((null value) '())
   ((consp value) (let ((x (car value))
			(xs (cdr value)))
		    (cond
		     ((consp x) (cons (proc-property-value x)
				      (proc-property-value xs)))
		     ((eq x 'eval) (eval (car xs)))
		     (t (cons x (proc-property-value xs))))))
   (t (list value))))

;;(insert (propertize "q" 'display '((margin right-margin) "asd")))

(defun sg-maybe-set-modified ()
  (when (and (local-variable-p 'project-buffer-modified-p)
	     (not project-buffer-modified-p))
    (sg-call-procedure/call-callback ft:switch-on-indicator! '(0))
    (setq project-buffer-modified-p t)))


(defun sg-set-unmodified ()
  (when (local-variable-p 'project-buffer-modified-p)
    (sg-call-procedure/call-callback ft:switch-off-indicator! '(0))
    (setq project-buffer-modified-p nil)))


(defadvice insert (after modification-indicator-handler
			  (&rest args))
  (sg-maybe-set-modified))

(defadvice self-insert-command (after modification-indicator-handler
				       (n))
  (sg-maybe-set-modified))

(defadvice undo (after modification-indicator-handler
			(&optional arg))
  (if (buffer-modified-p)
      (sg-maybe-set-modified)
    (sg-set-unmodified)))


(ad-activate #'insert)
(ad-activate #'self-insert-command)
(ad-activate #'undo)

(add-hook 'after-save-hook #'sg-set-unmodified)


(defun ft:new-project-from-dir (name)
  (interactive "snew project name:")
  (sg-call-procedure/call-callback ft:new-project-from-dir (list name)))


(define-remote/interactive/callback-calling-procedures
  (ft:select-next
   ft:select-prev
   ft:select-next-4
   ft:select-prev-4
   ft:lift-current-node
   ft:lower-current-node))



(define-minor-mode sg-mode
  "??? mode"
  :init-value nil
  ;:lighter " CN"
  :keymap
  (let ((sg-map (make-sparse-keymap)))
    ;; ((fn-map (make-sparse-keymap)))
    ;; (ctrl-f-map (make-sparse-keymap))
    ;; (define-key ctrl-f-map (kbd "d") #'fn-remove-from-tree)
    ;; (define-key ctrl-f-map (kbd "M-d") #'fn-remove-file)
    ;; (define-key fn-map (kbd "C-f") ctrl-f-map)

    (define-key sg-map (kbd "s-[") #'ft:select-prev)
    (define-key sg-map (kbd "s-]") #'ft:select-next)
    (define-key sg-map (kbd "s-{") #'ft:select-prev-4)
    (define-key sg-map (kbd "s-}") #'ft:select-next-4)

    (define-key sg-map (kbd "M-s-[") #'ft:lift-current-node)
    (define-key sg-map (kbd "M-s-]") #'ft:lower-current-node)
    sg-map))


(global-set-key [s-f1] (lambda () (interactive) (sg-call-procedure/call-callback ft:load-project! '("python-epc"))))

(add-hook 'kill-emacs-hook (remote-method ft:cache-projects!))


;(cl-prettyprint (macroexpand '(call-sg-procedure qwe ())))

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







(defface sg-node-face 
  '(( t :family "Liberation Mono")) "")

(defface sg-leaf-face 
  '(( t :inherit sg-node-face :foreground "azure3")) "")

(defface sg-intr-face  
  '(( t :inherit sg-node-face :foreground "#5B55FE" :weight ultra-bold)) "")



(defun sg-server-restart ()
  (epc:stop-epc sg-epc)
  (sg-server-start))


(defun sg-server-start-or-restart ()
  (interactive)
  (if sg-epc
      (sg-server-restart)
    (sg-server-start)))


(defun insert-lambda ()
  (interactive)
  (insert "lambda"))

(sg-server-start)

(provide 'srcgraph)
