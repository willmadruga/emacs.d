;;;;; rational-addon-javascript.el --- Rational Addon for my javascript development environment  -*- lexical-binding: t; -*-

;; Commentary

;;; Code:

(rational-package-install-package 'lsp-mode)
(rational-package-install-package 'js2-mode)
(rational-package-install-package 'typescript-mode)
(rational-package-install-package 'flycheck)

(setq js-chain-indent t)
(setq js2-highlight-level 3)
(setq js2-highlight-external-variables t)
(setq js2-idle-timer-delay 0.1)

(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default js-indent-first-init nil)
(setq-default js-indent-align-list-continuation nil)

(add-to-list 'auto-mode-alist '("\\.ts$"  . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$"  . js2-mode))

(add-hook 'js-mode-hook 'lsp)

(provide 'rational-addon-javascript)
;;; rational-addon-javascript.el ends here
