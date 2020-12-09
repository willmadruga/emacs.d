;; wmad-devel-el.el --- Development Configuration Layer : Emacs-Lisp

;;; Commentary:

;;; Code:

(defun wmad/devel-elisp-init ()

  ;; suggest ;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package suggest
    :ensure t
    :config
    (add-hook 'emacs-lisp-mode-hook
              #'smartparens-mode))

  ;; string library ;;;;;;;
  (use-package s :ensure t)

  ;; dash - modern list library
  (use-package dash :ensure t)

  )

(provide 'wmad-devel-el)
;;; wmad-devel-el.el ends here
