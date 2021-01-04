;; wmad-devel-lisp.el --- Development Configuration Layer : Common Lisp, Scheme -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-lisp-init ()
  "LISP configuration."

  ;; slime ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package slime
    :config
    (setq inferior-lisp-program "sbcl")
    (setq scheme-program-name "scheme"))


  ;; TODO try out sly: https://github.com/joaotavora/sly
  ;; (use-package sly)

  )

(provide 'wmad-devel-lisp)
;;; wmad-devel-lisp.el ends here
