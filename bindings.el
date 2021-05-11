;;; bindings.el --- Bindings related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-m"))


(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)


(global-set-key (kbd "C-s")   'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)


(global-set-key (kbd "s-<right>")   'windmove-right)
(global-set-key (kbd "s-<left>")    'windmove-left)
(global-set-key (kbd "s-<up>")      'windmove-up)
(global-set-key (kbd "s-<down>")    'windmove-down)


(global-set-key (kbd "C-s-<down>")  'enlarge-window)
(global-set-key (kbd "C-s-<up>")    'shrink-window)
(global-set-key (kbd "C-s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)


(global-set-key (kbd "C-c <down>") 'wmad/duplicate-line)


;; Netsuite SDFCLI wrapper - temporary lib I am working on, name is likely to change.
(global-set-key (kbd "C-c n c") 'netsuite/create-project)
(global-set-key (kbd "C-c n d") 'netsuite/deploy)
(global-set-key (kbd "C-c n u") 'netsuite/upload-buffer)
(global-set-key (kbd "C-c n 1") 'netsuite/deploy21)
(global-set-key (kbd "C-c n 2") 'netsuite/upload-buffer21)


(wmad/if-package 'dumb-jump
                 (global-set-key (kbd "C-c g") 'dumb-jump-go)
                 (global-set-key (kbd "C-c b") 'dumb-jump-back))


(wmad/if-package 'undo-fu
                 (global-set-key (kbd "C-z") 'undo-fu-only-undo)
                 (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))


(wmad/if-package 'origami
                 (global-set-key (kbd "C-c z") 'origami-toggle-node))


(wmad/if-package 'browse-kill-ring
                 (global-set-key (kbd "M-y") 'browse-kill-ring))


(wmad/if-package 'swiper
                 (progn (global-set-key (kbd "C-s") 'swiper)))


(wmad/if-package 'hl-todo
                 (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
                 (define-key hl-todo-mode-map (kbd "C-c t") 'hl-todo-next)
                 (define-key hl-todo-mode-map (kbd "C-c c") 'hl-todo-occur)
                 (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert))


(wmad/if-package 'move-text
                 (move-text-default-bindings))


(wmad/if-package 'eglot
                 (define-key eglot-mode-map (kbd "M-m dd") 'eldoc)
                 (define-key eglot-mode-map (kbd "M-,") 'eglot-rename)
                 (define-key eglot-mode-map (kbd "M-=") 'eglot-format)
                 (define-key eglot-mode-map (kbd "M-?") 'xref-find-references)
                 (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
                 (define-key eglot-mode-map (kbd "M-/") 'completion-at-point))


(wmad/if-package 'org-brain
                 (global-set-key (kbd "C-c o b") 'org-brain-goto)
                 (global-set-key (kbd "C-c o c") 'org-capture)
                 (global-set-key (kbd "C-c o a") 'org-brain-agenda))


(wmad/if-package 'olivetti-mode
                 (global-set-key (kbd "C-c w") 'olivetti-mode))


(wmad/if-package 'crux
                 (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
                 (global-set-key (kbd "C-o") 'crux-smart-open-line)
                 (global-set-key (kbd "C-c C-l") 'crux-duplicate-current-line-or-region)
                 (global-set-key (kbd "C-c C--") 'crux-kill-whole-line)
                 (global-set-key (kbd "C-c ;") 'crux-duplicate-and-comment-current-line-or-region))


(wmad/if-package 'counsel
                 (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
                 (global-set-key (kbd "M-x") 'counsel-M-x)
                 (global-set-key (kbd "C-x C-f") 'counsel-find-file)
                 (global-set-key (kbd "C-x C-/") 'counsel-imenu)
                 (global-set-key (kbd "C-c r") 'counsel-recentf))


;;; bindings.el ends here
