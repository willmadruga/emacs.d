;; wmad-devel-el.el --- Development Configuration Layer : Emacs-Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-elisp-init ()
  "Elisp configuration."
  
  ;; suggest ;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package suggest
    :defer t
    :ensure t
    :config
    (add-hook 'emacs-lisp-mode-hook
              #'smartparens-mode))

  ;; string library ;;;;;;;
  (use-package s
    :defer t
    :ensure t)

  ;; dash - modern list library
  (use-package dash
    :defer t
    :ensure t)

  )

(provide 'wmad-devel-el)
;;; wmad-devel-el.el ends here
