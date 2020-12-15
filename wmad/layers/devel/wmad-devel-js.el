;; wmad-devel-js.el --- Development Configuration Layer : Javascript
;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-js-init ()

  ;; js mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package js2-mode
    :defer t
    :ensure t
    :config
    (setq js-indent-level 2)

    ;; disable jshint
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))

    ;; enable eslint
    (flycheck-add-mode 'javascript-eslint 'js2-mode)

    (setq-default flycheck-temp-prefix ".flycheck")
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist)))

    (add-hook 'js-mode-hook #'smartparens-mode)
    (add-hook 'js2-mode-hook 'lsp-deferred)
    (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration))

  ;; js refactor ;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package js2-refactor
    :defer t
    :ensure t
    :config (add-hook 'js2-mode-hook
                      #'js2-refactor-mode))

  ;; "Netsuite-Mode" package: name is yet to be defined.
  ;; that's my package, basically a wrapper around SDFCLI
  (use-package emacs
    :config
    (load-file "~/src/netsuite-mode.el/netsuite.el")
    (add-to-list 'auto-mode-alist
                 '("\\.js\\'" . netsuite-mode)
                 '("\\.js\\'" . display-fill-column-indicator-mode))
    (setq fill-column 140))

  )

(provide 'wmad-devel-js)
;;; wmad-devel-js.el ends here
