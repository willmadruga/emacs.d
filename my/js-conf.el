;;; js-conf.el --- Javascript IDE config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(use-package eglot
  :ensure t
  :defer t
  :config
  (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio"))))

;; FIXME: eglot not initiated when I open a js file.

(use-package js2-mode
  :ensure t
  :diminish
  :mode "\\.js\\'"
  :after (eglot flycheck)
  :hook ((js2-mode-hook . flycheck-mode)
         (js2-mode-hook . show-paren-mode)
         (js2-mode-hook . eglot-ensure))

  :config
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

  ;; still not loading :/
  ;; need some hacking to understand why..
  ;; meanwhile I can manually start an eglot session...
  (add-hook 'js2-mode-hook 'eglot-ensure))


(use-package flycheck
  :ensure t
  :diminish
  :hook (prog-mode-hook . global-flycheck-mode)
  :config

  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-javascript-eslint-executable "~/.npm-packages/bin/eslint")

  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)
                        '(json-jsonlist)))

  (flycheck-add-mode 'javascript-eslint 'js2-mode))

;;; js-conf.el ends here
