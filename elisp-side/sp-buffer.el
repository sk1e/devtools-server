;;; -*- lexical-binding: t -*-

(defun insert-to (string buffer)
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      (insert string))))


(defun insert-to-propertized (string buffer properties)
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      (insert (apply #'propertize/eval string properties)))))


(defun insert-to-at-point (string buffer point)
  (with-current-buffer buffer
    (goto-char point)
    (let ((buffer-read-only nil))
      (insert (string)))))


(defun insert-to-propertized-at-point (string buffer properties point)
  (with-current-buffer buffer
    (goto-char point)
    (let ((buffer-read-only nil))
      (insert (apply #'propertize string properties)))))

(defun append-to-header-propertized (string buffer properties)
  (with-current-buffer buffer
    (setq header-line-format (concat header-line-format (apply #'propertize string properties)))))


(defun call-in-buffer (buffer proc &optional args)
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      (apply proc args))))

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

(provide 'sp-buffer)


