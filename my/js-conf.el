;;; js-conf.el --- Javascript IDE config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(setup (:package eglot)
  (require 'eglot)
  (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio"))))

(setup (:package js2-refactor))

(setup (:package js2-mode)
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

  (:hook flycheck-mode)
  (:hook show-paren-mode)
  (:hook eglot-ensure)
  (:hook js2-refactor-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(setup (:package flycheck)
  (require 'flycheck)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq flycheck-javascript-eslint-executable "~/.npm-packages/bin/eslint")

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)
                        '(json-jsonlist)))

  (setq flycheck-highlighting-mode 'lines)
  (setq-default flycheck-temp-prefix ".flycheck")
  (flycheck-add-mode 'javascript-eslint 'js2-mode)

  (:with-mode prog-mode
    (:hook global-flycheck-mode)))

(setup (:package tree-sitter)
  (:with-mode js-mode
    (:hook tree-sitter-mode)))
(setup (:package tree-sitter-langs))

;; TODO: Proper setup when it is moved into melpa.
;; https://github.com/isamert/jsdoc.el
(add-to-list 'load-path "/home/wmadruga/src/github/jsdoc.el")
(require 'jsdoc)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; js-conf.el ends here
