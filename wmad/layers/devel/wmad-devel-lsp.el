;; wmad-devel-lsp.el --- Development Configuration Layer : LSP
;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))


(defun wmad/devel-lsp-init ()

  ;; lsp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package lsp-mode
    :defer t
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (lsp-mode . efs/lsp-mode-setup)
    :init
    (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
    :config
    (lsp-enable-which-key-integration t))

  ;; company lsp mode ;;;;;;;;;;;;;;;
  (use-package company-lsp
    :defer t
    :ensure t)

  ;; lsp UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package lsp-ui
    :defer t
    :ensure t
    :hook (lsp-mode-hook . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))

  ;; eglot - client for LSP servers
  (use-package eglot
    :defer t
    :ensure t)

  )

(provide 'wmad-devel-lsp)
;;; wmad-devel-lsp.el ends here