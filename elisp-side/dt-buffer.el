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
      (insert string))))


(defun insert-to-propertized-at-point (string buffer properties point)
  (with-current-buffer buffer
    (goto-char point)
    (let ((buffer-read-only nil))
      (insert (apply #'propertize string properties)))))


(defun set-buffer-header (buffer header)
  (with-current-buffer buffer
    (setq header-line-format header)))



(defun call-in-buffer (buffer proc &optional args)
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      (apply proc args))))

;; TOFIX dirty things

(provide 'dt-buffer)


