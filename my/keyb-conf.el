;;; keyb-conf.el --- Key Bindings config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-S-z"))

(move-text-default-bindings)


(global-set-key (kbd "C-c c a") 'consult-apropos)
(global-set-key (kbd "C-c c b") 'consult-buffer)
(global-set-key (kbd "C-c c c") 'consult-flycheck)
(global-set-key (kbd "C-c c f") 'consult-find)
(global-set-key (kbd "C-c c g") 'consult-grep)
(global-set-key (kbd "C-c c l") 'consult-line)
(global-set-key (kbd "C-c c m") 'consult-imenu)
(global-set-key (kbd "C-c c r") 'consult-ripgrep)
(global-set-key (kbd "C-c c R") 'consult-recent-file)
(global-set-key (kbd "C-c c s") 'consult-isearch)


(global-set-key (kbd "C-c d d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c d ;") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c d i") 'devdocs-browser-install-doc)
(global-set-key (kbd "C-c d l") 'devdocs-browser-list-docs)
(global-set-key (kbd "C-c d b") 'devdocs-browser-open)
(global-set-key (kbd "C-c d w") 'wdired-change-to-wdired-mode)


(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e d") 'eglot-find-declaration)
(global-set-key (kbd "C-c e e") 'eval-last-sexp)
(global-set-key (kbd "C-c e f") 'eglot-format)
(global-set-key (kbd "C-c e i") 'eglot-find-implementation)
(global-set-key (kbd "C-c e p") 'pp-eval-last-sexp)
(global-set-key (kbd "C-c e r") 'eglot-rename)


(global-set-key (kbd "C-c g g") 'dumb-jump-go)
(global-set-key (kbd "C-c g b") 'dumb-jump-back)


(global-set-key (kbd "C-c n c") 'netsuite/create-project)
(global-set-key (kbd "C-c n d") 'netsuite/deploy)
(global-set-key (kbd "C-c n u") 'netsuite/upload-buffer)


(global-set-key (kbd "C-c o d") 'org-roam-dailies-map)
(define-key org-roam-dailies-map (kbd "Y") 'org-roam-dailies-capture-yesterday)
(define-key org-roam-dailies-map (kbd "T") 'org-roam-dailies-capture-tomorrow)
(global-set-key (kbd "C-c o t") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c o f") 'org-roam-node-find)
(global-set-key (kbd "C-c o i") 'org-roam-node-insert)
(global-set-key (kbd "C-c o s") 'org-save-all-org-buffers)


(global-set-key (kbd "C-c p c") 'password-store-copy)
(global-set-key (kbd "C-c p f") 'password-store-copy-field)


(global-set-key (kbd "C-c m b") 'magit-blame)


(global-set-key (kbd "C-c t n")   'tab-new)
(global-set-key (kbd "C-c t r")   'tab-rename)
(global-set-key (kbd "C-c t c")   'tab-close)
(global-set-key (kbd "C-c t t")   'tab-recent)
(global-set-key (kbd "C-c t l")   'tab-list)
(global-set-key (kbd "C-c t SPC") 'toggle-frame-tab-bar)


(global-set-key (kbd "C-c t <right>") 'tab-next)
(global-set-key (kbd "C-c t <left>") 'tab-previous)
(global-set-key (kbd "C-c t S-<right>") 'tab-move)


(global-set-key (kbd "C-c D")   'dired)
(global-set-key (kbd "C-c i")   'ibuffer)
(global-set-key (kbd "C-c S")   'shell)
(global-set-key (kbd "C-c s")   'speedbar)
(global-set-key (kbd "C-c Y")   'yas-describe-tables)
(global-set-key (kbd "C-c y")   'yank-from-kill-ring)


(global-set-key (kbd "C-c <home>") 'org-timer-set-timer)
(global-set-key (kbd "C-c <end>")  'org-timer-pause-or-continue)


(global-set-key (kbd "C-c u u")    'popper-toggle-latest)
(global-set-key (kbd "C-c u c")    'popper-cycle)
(global-set-key (kbd "C-c u t")    'popper-toggle-type) ;; turn into a pop-up and back.


;; super+direction to work on ewwm-config.el
;; (global-set-key (kbd "C-c <up>")      'windmove-up)
;; (global-set-key (kbd "C-c <down>")    'windmove-down)
;; (global-set-key (kbd "C-c <left>")    'windmove-left)
;; (global-set-key (kbd "C-c <right>")   'windmove-right)


(global-set-key (kbd "C-c C-<up>")    'enlarge-window)
(global-set-key (kbd "C-c C-<down>")  'shrink-window)
(global-set-key (kbd "C-c C-<left>")  'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-<right>") 'shrink-window-horizontally)


(global-set-key (kbd "C-x C-a") 'eval-print-last-sexp)


;;; keyb-conf.el ends here
