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

;; (define-key evil-normal-state-map (kbd "a") 'function)
;; (define-key evil-insert-state-map (kbd "a") 'function)
;; (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)

;; TODO: define a different leader key for -nw because of my server...

;; (evil-set-leader 'normal (kbd "SPC"))
;; (evil-define-key 'normal 'global (kbd "<leader>w") 'evil-save)


;; (evil-set-leader 'insert (kbd "C-SPC"))
;; (evil-define-key 'insert 'global (kbd "<leader><down>") 'crux-duplicate-current-line-or-region)
;; (evil-define-key 'insert 'global (kbd "<leader><down>") 'crux-duplicate-current-line-or-region)

;; Leader Keys
;; !Insert Mode: space
;; Insert Mode: C-space

 ;; avy-goto-char-timer)
 ;; popper-toggle-latest)
 ;; ibuffer)
 ;; embark-act)
 ;; consult-buffer)
 ;; org-roam-dailies-map)
 ;; crux-duplicate-current-line-or-region)

;;  consult-apropos)
;;  consult-buffer)
;;  consult-flycheck)
;;  consult-find)
;;  consult-grep)
;;  consult-line)
;;  consult-imenu)
;;  consult-imenu-multi)
;;  consult-ripgrep)
;;  consult-recent-file)
;;  consult-isearch)
;;  embark-prefix-help-command)

;;  eglot-format)
;;  eglot-rename)
;;  eglot-shutdown)
;;  js2-mode-toggle-element)
;;  embark-prefix-help-command)

;;  dumb-jump-go)
;;  dumb-jump-back)
;;  netsuite/create-project)
;;  netsuite/deploy)
;;  netsuite/upload-buffer)
;;  embark-prefix-help-command)

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
