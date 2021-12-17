;;; keyb-conf.el --- Key Bindings config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'org-roam)
(require 'org-roam-dailies)
(require 'eglot)
(require 'js2-mode)
(require 'elfeed)
(require 'sdfcli "../../my/sdfcli.el")

(global-set-key (kbd "s-a") 'avy-goto-char-timer)
(global-set-key (kbd "s-b") 'consult-buffer)
(global-set-key (kbd "s-B") 'ibuffer)
(global-set-key (kbd "s-<down>") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "s-D") 'netsuite/deploy)
(global-set-key (kbd "s-e") 'embark-act)
(global-set-key (kbd "s-E") 'elfeed)
(global-set-key (kbd "s-f") 'eglot-format)
(global-set-key (kbd "s-g") 'consult-ripgrep)
(global-set-key (kbd "s-i") 'imenu)
(global-set-key (kbd "s-j") 'jsdoc)
(global-set-key (kbd "s-o") 'org-roam-dailies-map)
(global-set-key (kbd "s-O") 'org-roam-node-find)
(global-set-key (kbd "s-r") 'eglot-rename)
(global-set-key (kbd "s-t") 'ansi-term)
(global-set-key (kbd "s-u") 'netsuite/upload-buffer)
(global-set-key (kbd "s-w") 'wdired-change-to-wdired-mode)
(global-set-key (kbd "s-z") 'js2-mode-toggle-element)
(global-set-key (kbd "s-.") 'dumb-jump-go)
(global-set-key (kbd "s-,") 'dumb-jump-back)
(global-set-key (kbd "s-/") 'consult-line)


;;; keyb-conf.el ends here
