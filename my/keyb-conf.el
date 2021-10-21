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

(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-p") 'popper-toggle-latest)
(global-set-key (kbd "M-i") 'ibuffer)
(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "C-x C-b") 'consult-buffer)
(global-set-key (kbd "C-c o d") 'org-roam-dailies-map)
(global-set-key (kbd "C-M-<down>") 'crux-duplicate-current-line-or-region)

;; M-SPC is still my workflow prefix, it needs to work on EXWM windows
(global-set-key (kbd "M-SPC C-.") 'embark-prefix-help-command)

;; [C]onsult
(global-set-key (kbd "M-SPC c a")   'consult-apropos)
(global-set-key (kbd "M-SPC c b")   'consult-buffer)
(global-set-key (kbd "M-SPC c f")   'consult-flycheck)
(global-set-key (kbd "M-SPC c F")   'consult-find)
(global-set-key (kbd "M-SPC c g")   'consult-grep)
(global-set-key (kbd "M-SPC c l")   'consult-line)
(global-set-key (kbd "M-SPC c i")   'consult-imenu)
(global-set-key (kbd "M-SPC c I")   'consult-imenu-multi)
(global-set-key (kbd "M-SPC c r")   'consult-ripgrep)
(global-set-key (kbd "M-SPC c R")   'consult-recent-file)
(global-set-key (kbd "M-SPC c s")   'consult-isearch)
(global-set-key (kbd "M-SPC c C-.") 'embark-prefix-help-command)

;; [J]avascript Develoment
(global-set-key (kbd "M-SPC j f")   'eglot-format)
(global-set-key (kbd "M-SPC j r")   'eglot-rename)
(global-set-key (kbd "M-SPC j s")   'eglot-shutdown)
(global-set-key (kbd "M-SPC j g")   'dumb-jump-go)
(global-set-key (kbd "M-SPC j b")   'dumb-jump-back)
(global-set-key (kbd "M-SPC j h")   'js2-mode-toggle-element)
(global-set-key (kbd "M-SPC j c")   'netsuite/create-project)
(global-set-key (kbd "M-SPC j d")   'netsuite/deploy)
(global-set-key (kbd "M-SPC j u")   'netsuite/upload-buffer)
(global-set-key (kbd "M-SPC j C-.") 'embark-prefix-help-command)

;; [E]macs Misc
(global-set-key (kbd "M-SPC e a")   'ansi-term)
(global-set-key (kbd "M-SPC e f")   'elfeed)
(global-set-key (kbd "M-SPC e d")   'elfeed-dashboard)
(global-set-key (kbd "M-SPC e w")   'wdired-change-to-wdired-mode)
(global-set-key (kbd "M-SPC e v l") 'eval-last-sexp)
(global-set-key (kbd "M-SPC e v p") 'eval-print-last-sexp)
(global-set-key (kbd "M-SPC e C-.") 'embark-prefix-help-command)

;; [O]RG Mode
(global-set-key (kbd "M-SPC o a")     'org-agenda)
(global-set-key (kbd "M-SPC o f")     'org-roam-node-find)
(global-set-key (kbd "M-SPC o i")     'org-roam-node-insert)
(global-set-key (kbd "M-SPC o d")     'org-roam-dailies-map)
(global-set-key (kbd "M-SPC o d C-.") 'embark-prefix-help-command)
(global-set-key (kbd "M-SPC o C-.")   'embark-prefix-help-command)

;; [P]assword Store
(global-set-key (kbd "M-SPC p c")   'password-store-copy)
(global-set-key (kbd "M-SPC p f")   'password-store-copy-field)
(global-set-key (kbd "M-SPC p C-.") 'embark-prefix-help-command)

;; e[X]tras
;; Other keybindings I usually access through embark-prefix-help-command right after M-SPC prefix
(global-set-key (kbd "M-SPC X w t") 'crux-transpose-windows)
(global-set-key (kbd "M-SPC X p l") 'popper-toggle-latest)
(global-set-key (kbd "M-SPC X p t") 'popper-toggle-type)

;;; keyb-conf.el ends here
