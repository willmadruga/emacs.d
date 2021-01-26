;; wmad-devel-js.el --- Development Configuration Layer : Javascript -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-js-init ()
  "Javascript configuration."

  ;; js mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package js2-mode
    :mode "\\.m?js\\'"
    :interpreter "node"
    :commands js2-line-break
    :hook (js2-mode-hook . rainbow-delimiters-mode)
    :config
    (setq js-indent-level 2
          ;; Don't use built-in syntax checking
          js2-mode-show-strict-warnings nil
          ;; Don't mishighlight shebang lines
          js2-skip-preprocessor-directives t
          ;; let flycheck handle this
          js2-mode-show-parse-errors nil
          js2-mode-show-strict-warnings nil
          ;; Flycheck provides these features, so disable them: conflicting with
          ;; the eslint settings.
          js2-strict-trailing-comma-warning nil
          js2-strict-missing-semi-warning nil
          ;; maximum fontification
          js2-highlight-level 3
          js2-highlight-external-variables t
          js2-idle-timer-delay 0.1
          js-chain-indent t))

  (use-package xref-js2
    :after (:or js2-mode)) ;; set it up => :xref-backend #'xref-js2-xref-backend

  ;; js refactor ;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package js2-refactor
    :hook (js2-mode-hook . js2-refactor-mode))

  ;; typescript
  (use-package typescript-mode
    :hook (typescript-mode . rainbow-delimiters-mode))

  (use-package tide
    :ensure t
    :after (typescript-mode company flycheck)
    :hook ((typescript-mode . tide-setup)
           (typescript-mode . tide-hl-identifier-mode)
           (before-save . tide-format-before-save))
    :config
    (setq tide-completion-detailed t
          tide-always-show-documentation t
          tide-server-max-response-length 524288))


  ;; "Netsuite-Mode" package: name is yet to be finalized.
  ;; (use-package netsuite-mode
  ;;   :straight (netsuite-mode
  ;;              :local-repo "~/src/netsuite-mode.el"
  ;;              :type git)
  (use-package emacs
    :config
    (load-file "~/src/netsuite-mode.el/netsuite.el")
    (add-to-list 'auto-mode-alist
                 '("\\.js\\'" . js2-mode)
                 '("\\.js\\'" . display-fill-column-indicator-mode))
    (setq fill-column 140))

  )

(provide 'wmad-devel-js)
;;; wmad-devel-js.el ends here
