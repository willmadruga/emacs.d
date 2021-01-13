;; wmad-devel-el.el --- Development Configuration Layer : Emacs-Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-elisp-init ()
  "Elisp configuration."
  
  ;; suggest ;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package suggest
    :config
    (add-hook 'emacs-lisp-mode-hook
              #'smartparens-mode))

  ;; string library ;;;;;;;
  (use-package s)

  ;; dash - modern list library
  (use-package dash)

  ;; It’s like jQuery, but way less useful
  ;; required by bmi, bookmark importer
  (use-package elquery)

  )

(provide 'wmad-devel-el)
;;; wmad-devel-el.el ends here
