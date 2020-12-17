;; wmad-devel-lsp.el --- Development Configuration Layer : LSP -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; (defun wmad/lsp-mode-setup ()
;;   "LSP-mode setup."

;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

(defun wmad/devel-lsp-init ()
  "LSP-mode configurations."

  (use-package lsp-mode
    :commands lsp
    :hook ((typescript-mode js2-mode) . lsp)
    ;; :bind (:map lsp-mode-map
    ;;             ("TAB" . completion-at-point))
    :config
    (setq lsp-session-file (concat user-emacs-directory "lsp-sessions"))
    (setq lsp-headerline-breadcrumb-enable t)
    )
  
  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :config
    (setq lsp-ui-sideline-enable t))

  
  ;;   ;; lsp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;   (use-package lsp-mode
  ;;     :ensure t
  ;;     :commands (lsp lsp-deferred)
  ;;     :hook (
  ;;            (lsp-mode-hook . wmad/lsp-mode-setup)
  ;;            (js2-mode-hook . lsp-deferred)
  ;;            (lsp-mode-hook . lsp-enable-which-key-integration)
  ;;            )
  ;;     :init
  ;;     (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  ;;     :config
  ;;     (lsp-enable-which-key-integration t))

  ;;   ;; company lsp mode ;;;;;;;;;;;;;;;
  ;;   (use-package company-lsp
  ;;     :defer t
  ;;     :ensure t)

  ;;   ;; lsp UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;   (use-package lsp-ui
  ;;     :ensure t
  ;;     :commands lsp-ui-mode
  ;;     :hook (lsp-mode-hook . lsp-ui-mode)
  ;;     :custom
  ;;     (lsp-ui-doc-position 'bottom))

  ;;   ;; eglot - client for LSP servers
  ;;   ;; (use-package eglot
  ;;   ;;   :ensure t)

  )

(provide 'wmad-devel-lsp)
;;; wmad-devel-lsp.el ends here
