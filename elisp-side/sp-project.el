;;; -*- lexical-binding: t -*-

;; pt - project tree

(require 'dt-utils)

(defconst dt:projects-path  "~/Projects")

(dt:define-remote/interactive
 pt:new-project!
 pt:select-by-name!
 
 pt:select-next-leaf!
 pt:select-prev-leaf!
 pt:select-next-leaf-4!
 pt:select-prev-leaf-4!
 
 pt:select-next-intr!
 pt:select-prev-intr!
 
 pt:lift-current-node!
 pt:lower-current-node!

 pt:rename!
 
 pt:remove-from-tree!
 pt:delete!

 pt:create-test!
 pt:toggle-test!
 pt:delete-test!
 
 pt:run-test-at-background!
 pt:run-module-at-foreground!
 pt:interrupt-execution!
 
 pt:initialize-git-repository!
 pt:switch-to-current-project-node!
 )

(defface pt:leaf-face 
  '(( t :inherit dt:ebuffer-node-face :foreground "azure3")) "")

(defface pt:intr-face  
  '(( t :inherit dt:ebuffer-node-face :foreground "#5B55FE" :weight ultra-bold)) "")



(defface pt:root-unmerged-face  
  '(( t :inherit dt:header-face :foreground "Orange")) "")




(define-minor-mode pt-mode
  "project tree mode"
  :init-value nil
					;:lighter " CN"
  :keymap
  (let ((pt-map (make-sparse-keymap)))
    ;; ((fn-map (make-sparse-keymap)))
    ;; (ctrl-f-map (make-sparse-keymap))
    ;; (define-key ctrl-f-map (kbd "d") #'fn-remove-from-tree)
    ;; (define-key ctrl-f-map (kbd "M-d") #'fn-remove-file)
    ;; (define-key fn-map (kbd "C-f") ctrl-f-map)

    (define-key pt-map (kbd "s-[") #'pt:select-prev-leaf!)
    (define-key pt-map (kbd "s-]") #'pt:select-next-leaf!)
    (define-key pt-map (kbd "s-{") #'pt:select-prev-leaf-4!)
    (define-key pt-map (kbd "s-}") #'pt:select-next-leaf-4!)

    (define-key pt-map (kbd "M-s-[") #'pt:lift-current-node!)
    (define-key pt-map (kbd "M-s-]") #'pt:lower-current-node!)
    
    (define-key pt-map (kbd "s-<tab>") #'pt:toggle-test!)
    
    (define-key pt-map (kbd "s-\\") #'pt:run-module-at-foreground!)
    (define-key pt-map (kbd "s-|") #'pt:run-test-at-background!)
    (define-key pt-map (kbd "C-\\") #'pt:interrupt-execution!)
    
    (define-key pt-map (kbd "s-g") #'gt:switch-to-git-tree-buffer!)
    (define-key pt-map (kbd "M-c") #'gt:commit!)
    pt-map)
  (set (make-local-variable 'project-buffer-modified-p) nil))





(defun pt:new-project-from-existing-dir! ()
  (interactive)
  (let ((default-directory pt:projects-path)
	(insert-default-directory nil))
    (serp:call-remote-procedure pt:new-project-from-existing-dir!
				(list (directory-file-name (read-directory-name "new project name: "))))))

(defun pt:add-directory! ()
  (interactive)
  (let ((default-directory (serp:call-remote-procedure 'pt:entered-directory-path))
	(insert-default-directory nil))
    (serp:call-remote-procedure pt:add-directory!
				(list (directory-file-name (read-directory-name "new directory name: "))))))

(defun pt:add-file! ()
  (interactive)
  (let ((default-directory (serp:call-remote-procedure 'pt:entered-directory-path))
	(insert-default-directory nil))
    (serp:call-remote-procedure pt:add-file!
				(list (read-file-name "new file name: ")))))



(global-set-key [f1] (lambda () (interactive) (serp:call-remote-procedure pt:load-project! '("serp-racket"))))
(global-set-key [f2] (lambda () (interactive) (serp:call-remote-procedure pt:load-project! '("serp"))))
(global-set-key [f3] (lambda () (interactive) (serp:call-remote-procedure pt:load-project! '("devtools"))))
;; (global-set-key [f3] (lambda () (interactive) (serp:call-remote-procedure pt:load-project! '("emacs-extensions"))))

(add-hook 'kill-emacs-hook pt:cache-projects!)




(defvar pt:mod-status-updater (run-at-time nil 0.1 #'pt:update-modification-status))
(defun pt:update-modification-status ()
  (let ((modifiedp (buffer-modified-p)))
    (when (and (local-variable-p 'project-buffer-modified-p)
	       (not (eq project-buffer-modified-p modifiedp)))
      (setq project-buffer-modified-p modifiedp)
      (if modifiedp
	  (sp:call-sync/call-callback 'pt:switch-on-indicator! 0)
	(sp:call-sync/call-callback 'pt:switch-off-indicator! 0)))))


(defadvice switch-to-buffer (before pt-hook
				    (&rest args))
  (pt:update-modification-status)
  )

;; (defadvice compile-goto-error (after pt-hook
;; 				     (&rest args))
;;   (message ">>>> %s" (buffer-name (current-buffer)))

;;   (when pt-mode
;;     (serp:call-remote-procedure pt:select-by-name!
;; 				     (list (buffer-name (current-buffer))))))



(ad-activate #'switch-to-buffer)
;; (ad-activate #'compile-goto-error) 



(defun pt:make-buffer (path)
  (let ((buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (pt-mode)
      (layout-mode))
    buffer))

(defun pt:init-file-buffer (path new-name new-mode-line-buffer-identification-parts)
  (pt:rename-file-buffer (pt:make-buffer path)
			 new-name
			 new-mode-line-buffer-identification-parts))


;; (defun pt:init-test-buffer (path)
;;   (with-current-buffer (find-file-noselect path)
;;     (pt-mode)
;;     (layout-mode)))




(defun pt:rename-file-buffer (buffer new-name new-mode-line-buffer-identification-parts)
  (with-current-buffer buffer
    (rename-buffer new-name)
    (setq mode-line-buffer-identification (cl-destructuring-bind ((base-string base-face)
								  (name-string name-face))
					      new-mode-line-buffer-identification-parts
					    (concat (propertize base-string 'font-lock-face base-face)
						    (propertize name-string 'font-lock-face name-face))))))

(defvar pt:exec-proc nil)
(defvar pt:proc-inspector-timer (run-at-time nil 0.1 #'pt:inspect-proc))
(cancel-timer pt:proc-inspector-timer)


(defun pt:inspect-proc ()
  (let ((status (process-status pt:exec-proc)))
    (case status
      (run nil)
      (exit (when pt:exec-proc
	      (serp:call-remote-procedure pt:on-exit-status!
					  (list (process-exit-status pt:exec-proc)))
	      (cancel-timer pt:proc-inspector-timer)
	      
	      ;; (with-current-buffer (process-name pt:exec-proc)
	      ;; 	(setq buffer-read-only t))
	      
	      (set1 pt:exec-proc nil)))
      (otherwise (serp:call-remote-procedure pt:on-unexpected-status!
					     (list status))))))


(defconst pt:racket-programm "~/dev/racket/racket/bin/racket")
(defconst pt:racket-background-command (format "sleep 0.1 && %s" pt:racket-programm))



(defun pt:racket-test-output-filter (proc out)
  (let ((buffer (get-buffer (process-name proc))))
    (with-current-buffer buffer
      (let ((buffer-read-only nil))
	
	(when (string-match (rx (1+ digit) " success(es) "
				(group (1+ digit)) " failure(s) "
				(group (1+ digit)) " error(s)") out)
	  (serp:call-remote-procedure pt:on-test-result!
				      (list (+ (string-to-int (match-string-no-properties 1 out))
					       (string-to-int (match-string-no-properties 2 out))))))
	
	(princ (format "%s\n" out) buffer)))))




(defun pt:execute-test-at-background (name path)
  (setq pt:exec-proc (start-process-shell-command name name (format "%s %s"
								    pt:racket-background-command
								    path)))

  (with-current-buffer name
    (let ((buffer-read-only nil))
      (princ "\n" (get-buffer name))))

  (set-process-filter pt:exec-proc #'pt:racket-test-output-filter)
  (timer-activate pt:proc-inspector-timer))

(defun pt:execute-test-at-foreground (name path)
  (pt:execute-test-at-background name path)
  (select-window (display-buffer name)))

(defun pt:execute-module-at-foreground (name path)
  (setq pt:exec-proc (start-process name name pt:racket-programm path))
  (timer-activate pt:proc-inspector-timer)
  
  (select-window (display-buffer name)))


(defun pt:interrupt-process ()
  (interrupt-process pt:exec-proc))

					;(kill-process)



(defun show-test-exec-buffer ()
  (interactive)
  (let ((buffer (get-buffer (format "*exec %s*" (sp:call-sync 'pt:test-buffer-name)))))
    (when buffer
      (let ((win (get-buffer-window buffer)))
	(cond
	 ((window-live-p win)
	  (delete-window win))
	 
	 (t (display-buffer buffer)))))))

(global-set-key (kbd "s-2") #'show-test-exec-buffer)


(defun pt:init-exec-buffer (name)
  (let ((exec-buffer (get-buffer name)))
    
    (when exec-buffer
      
      (when (window-live-p (get-buffer-window exec-buffer))
	(delete-window (get-buffer-window exec-buffer)))
      
      (kill-buffer exec-buffer))
    
    (with-current-buffer (generate-new-buffer name)
      (compilation-mode)
      (font-lock-mode))))


					;(ad-activate #'buffer-modified-p)
;; (ad-activate #'insert)
;; (ad-activate #'backward-delete-char-untabify)
;; ;; (ad-activate #'delete-region)
;; (ad-activate #'self-insert-command)
;; (ad-activate #'undo)

;;(add-hook 'after-save-hook #'pt:set-unmodified)

;; (add-hook 'scheme-mode-hook #'pt-mode)
;; (add-hook 'scheme-mode-hook #'layout-mode)


(provide 'sp-project)


