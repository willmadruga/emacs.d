;;; js.el --- Javascript related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(wmad/package-install 'js2-refactor)
(wmad/package-install 'fill-column-indicator)

(wmad/package-install 'eglot)
(require 'eglot)
(add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))

(wmad/package-install 'js2-mode)
(require 'js2-mode)
(setq js-chain-indent t)
(setq js2-skip-preprocessor-directives t)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-trailing-comma-warning nil)
(setq js2-strict-missing-semi-warning nil)
(setq js2-highlight-level 3)
(setq js2-highlight-external-variables t)
(setq js2-idle-timer-delay 0.1)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 140)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default js-indent-first-init nil)
(setq-default js-indent-align-list-continuation nil)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'js2-refactor-mode)
(add-hook 'js2-mode-hook 'show-paren-mode)
(add-hook 'js2-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook 'eglot-ensure)

(wmad/package-install 'flycheck)
(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)
(setq-default flycheck-temp-prefix ".flycheck")
(setq flycheck-javascript-eslint-executable "~/.npm-packages/bin/eslint")

(setq-default flycheck-disabled-checkers
		          (append flycheck-disabled-checkers
			                '(javascript-jshint)
			                '(json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'js2-mode)
(setq flycheck-highlighting-mode 'lines)
(setq-default flycheck-temp-prefix ".flycheck")

(add-hook 'prog-mode-hook 'global-flycheck-mode)

;;; js.el ends here
