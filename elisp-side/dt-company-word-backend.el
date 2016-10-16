(require 'cl-lib)
(require 'company)
 

(defun dt:company-word-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  
  (cl-case command
    (interactive (company-begin-backend 'dt:company-word-backend))
    (prefix (company-grab-symbol))
    (candidates
     (message "completions for %s - %s" arg (dt:call 'pt:complete-word arg))
     (dt:call 'pt:complete-word arg))))




;; ;; (defun dt:company-word-backend (command &optional arg &rest ignored)
;; ;;   (interactive (list 'interactive))
  
;; ;;   (cl-case command
;; ;;     (interactive (company-begin-backend 'dt:company-word-backend))
;; ;;     (prefix (company-grab-symbol))
;; ;;     (candidates '("locala" "localb" "localc" "localz" "locale"))))


;; (setq q company-backends)
;; (setq company-backends '(dt:company-word-backend))

;; (company-grab-symbol-cons)
;; (setq company-backends q)


;; (setq tt "company-quickhelp-frontend is a compiled Lisp function in
;; `company-quickhelp.el'.

;; (company-quickhelp-frontend COMMAND)

;; `company-mode' front-end showing documentation in a `pos-tip' popup.
;; ")

;; (let ((x-gtk-use-system-tooltips nil))
;;   (pos-tip-show tt))

;; (company-q)

;; (company-quickhelp-)

;; (let ((x-gtk-use-system-tooltips nil))
;;   (pos-tip-show "message message message message messagemessagemessagl
;; `message' message message messagql


;; qweeqwe"))

(provide 'dt-company-word-backend)
