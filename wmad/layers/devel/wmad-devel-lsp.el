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

  )

(provide 'wmad-devel-lsp)
;;; wmad-devel-lsp.el ends here
