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

;; avy
(global-set-key (kbd "C-c a a") 'avy-goto-char-timer)

;; consult
(global-set-key (kbd "C-c c b") 'consult-buffer)
(global-set-key (kbd "C-c c g") 'consult-ripgrep)
(global-set-key (kbd "C-c c l") 'consult-line)

;; i*
(global-set-key (kbd "C-c i b") 'ibuffer)
(global-set-key (kbd "C-c i m") 'imenu)

;; Netsuite
(global-set-key (kbd "C-c C-D") 'netsuite/deploy)
(global-set-key (kbd "C-c C-u") 'netsuite/upload-buffer)

;; Javascript IDE
(global-set-key (kbd "C-c j f") 'eglot-format)
(global-set-key (kbd "C-c j d") 'jsdoc)
(global-set-key (kbd "C-c j r") 'eglot-rename)
(global-set-key (kbd "C-c j t") 'js2-mode-toggle-element)

;; ORG
(global-set-key (kbd "C-c o d") 'org-roam-dailies-map)
(global-set-key (kbd "C-c o f") 'org-roam-node-find)

;; Misc
(global-set-key (kbd "C-c .")      'dumb-jump-go)
(global-set-key (kbd "C-c ,")      'dumb-jump-back)
(global-set-key (kbd "C-c e a")    'embark-act)
(global-set-key (kbd "C-c e e")    'elfeed)
(global-set-key (kbd "C-c t")      'ansi-term)
(global-set-key (kbd "C-c d w")    'wdired-change-to-wdired-mode)
(global-set-key (kbd "C-c <down>") 'crux-duplicate-current-line-or-region)


;;; keyb-conf.el ends here
