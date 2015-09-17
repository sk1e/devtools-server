;;; -*- lexical-binding: t -*-
;; gt - git tree

(define-remote/interactive/callback-calling-procedures
  (gt:switch-to-git-tree-buffer!
   gt:commit!
   
   gt:select-next-leaf!
   gt:select-prev-leaf!
   gt:select-next-intr!
   gt:select-prev-intr!
   
   gt:append-branch!
   gt:checkout!
   gt:checkout-master!
   gt:delete-branch!
   gt:merge!))

(defface gt:root-unmerged-face  
  '(( t :inherit sp:header-face :foreground "Orange")) "")

(defface gt:root-detached-face
  '(( t :inherit sp:header-face :foreground "DarkRed")) "")


(define-minor-mode gt-mode
  "project tree mode"
  :init-value nil
  ;:lighter " CN"
  :keymap
  (let ((gt-map (make-sparse-keymap)))

    (define-key gt-map (kbd "s-g") #'pt:switch-to-current-project-node!)
    
    (define-key gt-map (kbd "a") #'gt:select-prev-leaf!)
    (define-key gt-map (kbd "d") #'gt:select-next-leaf!)
    (define-key gt-map (kbd "s-a") #'gt:select-prev-intr!)
    (define-key gt-map (kbd "s-d") #'gt:select-next-intr!)
    
    (define-key gt-map (kbd "C-b") #'gt:append-branch!)
    (define-key gt-map (kbd "C-d") #'gt:delete-branch!)
    (define-key gt-map (kbd "C-c") #'gt:checkout!)
    (define-key gt-map (kbd "C-r") #'gt:checkout-master!)
    (define-key gt-map (kbd "C-m") #'gt:merge!)
    
    gt-map))


(defconst gt-buffer (generate-new-buffer "git-tree"))

(with-current-buffer gt-buffer
  (font-lock-mode 1)
  (gt-mode)
  (setq buffer-read-only t))


(provide 'sp-git)
