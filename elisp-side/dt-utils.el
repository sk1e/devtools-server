;;; -*- lexical-binding: t -*-


(defmacro  dt:define-remote/interactive (&rest names)
  (loop for name in names
	collect `(defun ,name ()
		   (interactive)
		   (serp:call/call-return dt:server ',name))
	into defs
	finally return (cons 'progn defs)))

(provide 'dt-utils)
