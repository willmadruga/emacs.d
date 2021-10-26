;;; keyb-conf.el --- Key Bindings config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'popper)
(require 'org-roam)
(require 'org-roam-dailies)
(require 'eglot)
(require 'js2-mode)
(require 'elfeed)
(require 'password-store "../../password-store.el")
(require 'sdfcli "../../my/sdfcli.el")

;; unset C-z annoyance
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-S-z"))

;; unset keybindings I don't usually use so it can be repurposed.
(global-unset-key (kbd "M-i"))             ;; originally: (tab-to-tab-stop)
(global-unset-key (kbd "M-'"))             ;; originally: (abbrev-prefix-mark)
(global-unset-key (kbd "C-M-<down>"))      ;; originally: (down-list)
(global-unset-key (kbd "M-SPC"))           ;; originally: (just-one-space)
(global-unset-key (kbd "C-x C-b"))         ;; originally: (list-buffers)
(global-unset-key (kbd "C-x \\"))         ;; originally: (activate-transient-input-method)

(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-p") 'popper-toggle-latest)
(global-set-key (kbd "M-i") 'ibuffer)
(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "C-x C-b") 'consult-buffer)
(global-set-key (kbd "C-c o d") 'org-roam-dailies-map)
(global-set-key (kbd "C-M-<down>") 'crux-duplicate-current-line-or-region)

(global-set-key (kbd "C-x \\ C-.") 'embark-prefix-help-command)

;; [C]onsult
(global-set-key (kbd "C-x \\ c a")   'consult-apropos)
(global-set-key (kbd "C-x \\ c b")   'consult-buffer)
(global-set-key (kbd "C-x \\ c f")   'consult-flycheck)
(global-set-key (kbd "C-x \\ c F")   'consult-find)
(global-set-key (kbd "C-x \\ c g")   'consult-grep)
(global-set-key (kbd "C-x \\ c l")   'consult-line)
(global-set-key (kbd "C-x \\ c i")   'consult-imenu)
(global-set-key (kbd "C-x \\ c I")   'consult-imenu-multi)
(global-set-key (kbd "C-x \\ c r")   'consult-ripgrep)
(global-set-key (kbd "C-x \\ c R")   'consult-recent-file)
(global-set-key (kbd "C-x \\ c s")   'consult-isearch)
(global-set-key (kbd "C-x \\ c C-.") 'embark-prefix-help-command)

;; [J]avascript Develoment
(global-set-key (kbd "C-x \\ j f")   'eglot-format)
(global-set-key (kbd "C-x \\ j r")   'eglot-rename)
(global-set-key (kbd "C-x \\ j s")   'eglot-shutdown)
(global-set-key (kbd "C-x \\ j g")   'dumb-jump-go)
(global-set-key (kbd "C-x \\ j b")   'dumb-jump-back)
(global-set-key (kbd "C-x \\ j h")   'js2-mode-toggle-element)
(global-set-key (kbd "C-x \\ j c")   'netsuite/create-project)
(global-set-key (kbd "C-x \\ j d")   'netsuite/deploy)
(global-set-key (kbd "C-x \\ j u")   'netsuite/upload-buffer)
(global-set-key (kbd "C-x \\ j C-.") 'embark-prefix-help-command)

;; [E]macs Misc
(global-set-key (kbd "C-x \\ e a")   'ansi-term)
(global-set-key (kbd "C-x \\ e f")   'elfeed)
(global-set-key (kbd "C-x \\ e d")   'elfeed-dashboard)
(global-set-key (kbd "C-x \\ e w")   'wdired-change-to-wdired-mode)
(global-set-key (kbd "C-x \\ e v l") 'eval-last-sexp)
(global-set-key (kbd "C-x \\ e v p") 'eval-print-last-sexp)
(global-set-key (kbd "C-x \\ e C-.") 'embark-prefix-help-command)

;; [O]RG Mode
(global-set-key (kbd "C-x \\ o a")     'org-agenda)
(global-set-key (kbd "C-x \\ o f")     'org-roam-node-find)
(global-set-key (kbd "C-x \\ o i")     'org-roam-node-insert)
(global-set-key (kbd "C-x \\ o d")     'org-roam-dailies-map)
(global-set-key (kbd "C-x \\ o d C-.") 'embark-prefix-help-command)
(global-set-key (kbd "C-x \\ o C-.")   'embark-prefix-help-command)

;; [P]assword Store
(global-set-key (kbd "C-x \\ p c")   'password-store-copy)
(global-set-key (kbd "C-x \\ p f")   'password-store-copy-field)
(global-set-key (kbd "C-x \\ p C-.") 'embark-prefix-help-command)

;; e[X]tras
;; Other keybindings I usually access through embark-prefix-help-command right after C-x \ prefix
(global-set-key (kbd "C-x \\ X w t") 'crux-transpose-windows)
(global-set-key (kbd "C-x \\ X p l") 'popper-toggle-latest)
(global-set-key (kbd "C-x \\ X p t") 'popper-toggle-type)

;;; keyb-conf.el ends here
