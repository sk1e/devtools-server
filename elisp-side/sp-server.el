;;; -*- lexical-binding: t -*-

(require 'epcs)

(setq epc:debug-out t)

(defvar sp:epc nil)
(defvar sp:client-proc-list nil)

(defun sp:server-start ()
  (setq epc:accept-process-timeout 2000)
  (setq sp:epc (epc:start-epc "~/racket/bin/racket" '("~/Projects/devtools/remote-procedures.rkt")))
  (setq epc:accept-process-timeout 100)
  (sp:call-procedure/call-callback init-indicator-symbols '())
  
  (loop for proc in sp:client-proc-list
  	do (epc:define-method sp:epc proc proc))
  )


;(epc:define-method)

(defun sp:server-restart ()
  (epc:stop-epc sp:epc)
  (sp:server-start))


(defun sp:server-start-or-restart ()
  (interactive)
  (if sp:epc
      (sp:server-restart)
    (sp:server-start)))


(defmacro  define-remote/interactive/callback-calling-procedures (names)
  (loop for name in names
	collect `(defun ,name () ;;,(intern (format "sp:%s" name)) ()
		   (interactive)
		   (sp:call-procedure/call-callback ,name ()))
	into defuns
	finally return `(progn ,@defuns)))



(defun sp:call-sync (name &rest args)
  (epc:call-sync sp:epc name args))

(defun sp:call-sync/call-callback (name &rest args)
  (call-procedure-list (epc:call-sync sp:epc name args)))



(defmacro  sp:call-procedure/call-callback (name args)
  ;; `(call-procedure-list (epc:call-sync sp:epc ',name ,args))
  
  `(deferred:$
     (epc:call-deferred sp:epc ',name ,args)
     
     (deferred:nextc it
       #'call-procedure-list)

     (deferred:error it
       (lambda (err) (message "%s" err))))

  
  )


(defun call-procedure-list (lst)

  (loop for (proc args) in lst
	;do (message ">> %s . %s" proc args)
	do (apply proc args)))




(defmacro  sp:client-procedures (&rest defuns)
  (loop for defun in defuns
	collect (second defun) into defun-names
	finally return `(progn (setq sp:client-proc-list ',defun-names)
			       ,@defuns)))





(defmacro  remote-method (name)
  `(lambda () (sp:call-procedure/call-callback ,name nil)))


(provide 'sp-server)

