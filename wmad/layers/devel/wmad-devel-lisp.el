;; wmad-devel-lisp.el --- Development Configuration Layer : Common Lisp, Scheme
;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-lisp-init ()

  ;; slime ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package slime
    :defer t
    :ensure t
    :config
    (setq inferior-lisp-program "sbcl")
    (setq scheme-program-name "scheme"))

  
  ;; TODO try out sly: https://github.com/joaotavora/sly 
  ;; (use-package sly :ensure t)
  
  )

(provide 'wmad-devel-lisp)
;;; wmad-devel-lisp.el ends here
