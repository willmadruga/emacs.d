;; wmad-base-pkgs.el --- Emacs Configuration Layer: Base packages

;;; Commentary:

;;; Code:

(defun wmad/base-packages-init ()

  ;; Bufler ;;;;;;;;;;;;;;;;;;;
  (use-package bufler
    :ensure t
    :bind (("C-x b" . bufler)))

  ;; no-littering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package no-littering
    :ensure t
    :config
    (require 'recentf)

    (defvar recentf-exclude)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

  ;; amx ;;;;;;;;;;;;;;
  (use-package amx
    :ensure t
    :config (amx-mode))

  ;; undo-fu ;;;;;;;;;
  (use-package undo-fu
    :ensure t)

  ;; which-key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

  ;; diminish ;;;;;;;;;
  (use-package diminish
    :ensure t
    :after use-package)

  ;; try ;;;;;;;;;
  (use-package try
    :ensure t)

  ;; restart-emacs ;;;;;;;;;
  (use-package restart-emacs
    :ensure t )

  ;; helpful ;;;;;;;;;
  (use-package helpful
    :ensure t)

  ;; switch-window ;;;;;;;;;
  (use-package switch-window
    :ensure t)

  ;; Company ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package company
    :ensure t
    :after lsp-mode
    :bind (:map company-active-map
                ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))

  (add-hook 'after-init-hook 'global-company-mode)

  ;; company-box ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode))

  ;; vterm ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package vterm
    :ensure t
    :commands vterm
    :config
    (setq vterm-shell "zsh")
    (setq vterm-max-scrollback 10000))

  )

(provide 'wmad-base-pkgs)
;;; wmad-base-pkgs.el ends here
