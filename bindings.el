;;; bindings.el --- Bindings related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-m"))


(global-set-key (kbd "C-x <home>") 'wmad/open-dashboard)
(global-set-key (kbd "C-x <end>")  'ibuffer)
(global-set-key (kbd "C-x C-b")    'ibuffer)
(global-set-key (kbd "M-/")        'hippie-expand)


(global-set-key (kbd "C-s")   'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)


(global-set-key (kbd "s-<right>")   'windmove-right)
(global-set-key (kbd "C-x <next>")  'windmove-right)
(global-set-key (kbd "s-<left>")    'windmove-left)
(global-set-key (kbd "C-x <prior>") 'windmove-left)
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


(wmad/if-package 'projectile
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))


(wmad/if-package 'dumb-jump
  (global-set-key (kbd "C-c g") 'dumb-jump-go)
  (global-set-key (kbd "C-c b") 'dumb-jump-back))


(wmad/if-package 'undo-fu
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))


(wmad/if-package 'origami
  (global-set-key (kbd "C-c z") 'origami-toggle-node))


(wmad/if-package 'hl-todo
  (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c t") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c c") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert))


(wmad/if-package 'move-text
  (move-text-default-bindings))


(wmad/if-package 'lsp-mode
  ;; TODO: explore what's available
  ;; https://emacs-lsp.github.io/lsp-mode/page/keybindings/
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  ;; TODO: shorter keybindings for commands I use the most, such as lsp-rename
  )


(wmad/if-package 'org-brain
  (global-set-key (kbd "C-c o b") 'org-brain-goto)
  (global-set-key (kbd "C-c o c") 'org-capture)
  (global-set-key (kbd "C-c o a") 'org-brain-agenda))


(wmad/if-package 'olivetti-mode
  (global-set-key (kbd "C-c w") 'olivetti-mode))


(wmad/if-package 'crux
  (global-set-key (kbd "C-a")     'crux-move-beginning-of-line)
  (global-set-key (kbd "C-o")     'crux-smart-open-line)
  (global-set-key (kbd "C-c C-l") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c C--") 'crux-kill-whole-line)
  (global-set-key (kbd "C-c ;")   'crux-duplicate-and-comment-current-line-or-region))


(wmad/if-package 'consult
  (global-set-key (kbd "C-r")      'consult-ripgrep)
  (global-set-key (kbd "C-s")      'consult-line)
  (global-set-key (kbd "C-x b")    'consult-buffer)
  (global-set-key (kbd "C-x 4 b")  'consult-buffer-other-window)
  (global-set-key (kbd "C-x 5 b")  'consult-buffer-other-frame)


  (global-set-key (kbd "C-c o h")  'consult-org-heading)
  (global-set-key (kbd "C-c o g")  'consult-org-agenda)

  (global-set-key (kbd "M-y")    'consult-yank-pop)
  (global-set-key (kbd "M-g m")  'consult-mark)
  (global-set-key (kbd "M-g i")  'consult-imenu)
  (global-set-key (kbd "M-g I")  'consult-project-imenu)
  (global-set-key (kbd "M-g f")  'consult-flycheck)


  (global-set-key (kbd "M-s R") 'consult-recent-file)
  (global-set-key (kbd "M-s a") 'consult-apropos)
  (global-set-key (kbd "M-s f") 'consult-find)
  (global-set-key (kbd "M-s g") 'consult-grep)
  (global-set-key (kbd "M-s r") 'consult-ripgrep)
  (global-set-key (kbd "M-s l") 'consult-line)
  (global-set-key (kbd "M-s e") 'consult-isearch)


  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))

  (consult-customize consult-line :keymap my-consult-line-map))



(wmad/if-package 'yasnippet-snippets
  (global-set-key (kbd "C-h y") 'yas-describe-tables))



(wmad/if-package 'password-store
  (global-set-key (kbd "C-x \\ c") 'password-store-copy)
  (global-set-key (kbd "C-x \\ C") 'password-store-clear)
  (global-set-key (kbd "C-x \\ e") 'password-store-edit)
  (global-set-key (kbd "C-x \\ i") 'password-store-insert)
  (global-set-key (kbd "C-x \\ r") 'password-store-remove)
  (global-set-key (kbd "C-x \\ R") 'password-store-rename)
  (global-set-key (kbd "C-x \\ g") 'password-store-generate)
  (global-set-key (kbd "C-x \\ f") 'password-store-copy-field)
  (global-set-key (kbd "C-x \\ u") 'password-store-url))


;;; bindings.el ends here
