;;; keyb-conf.el --- Key Bindings config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'evil)
(require 'popper)
(require 'org-roam)
(require 'org-roam-dailies)
(require 'eglot)
(require 'js2-mode)
(require 'elfeed)
(require 'password-store "../../password-store.el")
(require 'sdfcli "../../my/sdfcli.el")

;; I should read the documentation more often, just saying...
;; https://evil.readthedocs.io/en/latest/overview.html
;; https://github.com/emacs-evil/evil-collection

(global-unset-key (kbd "M-SPC"))
(evil-set-leader 'normal (kbd "M-SPC"))
(evil-set-leader 'insert (kbd "M-SPC"))

(evil-define-key '(normal) 'global (kbd "<leader>od") 'org-roam-dailies-map)
(evil-define-key '(normal) 'global (kbd "<leader>of") 'org-roam-node-find)
(evil-define-key '(normal) 'global (kbd "<leader>e") 'embark-act)

(evil-define-key '(insert) 'global (kbd "<leader>ef") 'eglot-format)
(evil-define-key '(insert) 'global (kbd "<leader>er") 'eglot-rename)
(evil-define-key '(insert) 'global (kbd "<leader>es") 'eglot-shutdown)
(evil-define-key '(insert) 'global (kbd "<leader>za") 'js2-mode-toggle-element)

(evil-define-key '(normal insert) 'global (kbd "<leader>a") 'avy-goto-char-timer)
(evil-define-key '(normal insert) 'global (kbd "<leader>b") 'consult-buffer)
(evil-define-key '(normal insert) 'global (kbd "<leader>f") 'consult-line)
(evil-define-key '(normal insert) 'global (kbd "<leader>g") 'consult-ripgrep)
(evil-define-key '(normal insert) 'global (kbd "<leader>i") 'imenu)
(evil-define-key '(normal insert) 'global (kbd "<leader>j") 'crux-duplicate-current-line-or-region)
(evil-define-key '(normal insert) 'global (kbd "<leader>p") 'popper-toggle-latest)

(evil-define-key '(normal insert) 'global (kbd "<leader>.") 'dumb-jump-go)
(evil-define-key '(normal insert) 'global (kbd "<leader>,") 'dumb-jump-back)

(evil-define-key '(normal insert) 'global (kbd "<leader>nc") 'netsuite/create-project)
(evil-define-key '(normal insert) 'global (kbd "<leader>nd") 'netsuite/deploy)
(evil-define-key '(normal insert) 'global (kbd "<leader>nD") 'netsuite/deploy21)
(evil-define-key '(normal insert) 'global (kbd "<leader>nu") 'netsuite/upload-buffer)
(evil-define-key '(normal insert) 'global (kbd "<leader>nU") 'netsuite/upload-buffer21)

 ;; ansi-term)
 ;; elfeed)
 ;; elfeed-dashboard)
 ;; wdired-change-to-wdired-mode)
 ;; eval-last-sexp)
 ;; eval-print-last-sexp)
 ;; embark-prefix-help-command)

 ;; org-agenda)
 ;; org-roam-node-find)
 ;; org-roam-node-insert)
 ;; org-roam-dailies-map)
 ;; embark-prefix-help-command)
 ;; embark-prefix-help-command)

 ;; password-store-copy)
 ;; password-store-copy-field)
 ;; embark-prefix-help-command)

 ;; crux-transpose-windows)
 ;; popper-toggle-latest)
 ;; popper-toggle-type)

;;; keyb-conf.el ends here
