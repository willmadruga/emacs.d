(load "~/.emacs.d/elisp/private.el")
(load "~/.emacs.d/elisp/proxy.el")

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/emacs_saves/" t)))

(setq gc-cons-threshold (* 50 1000 1000))

(use-package emacs
  :config
  (defun prot/rebuild-emacs-init ()
    "Produce Elisp init from my Org dotemacs.
    Add this to `kill-emacs-hook', to use the newest file in the next
    session. The idea is to reduce startup time, though just by
    rolling it over to the end of a session rather than the beginning
    of it."
    (let ((init-el "~/.emacs.d/emacs-init.el")
          (init-org "~/.emacs.d/emacs-init.org"))
      (when (file-exists-p init-el)
        (delete-file init-el))
      (org-babel-tangle-file init-org init-el)))
  :hook ((kill-emacs-hook . prot/rebuild-emacs-init)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(delete-selection-mode 1)
(add-to-list 'exec-path "~/bin")
(setenv "BROWSER" "firefox")

(setq undo-limit 80000000)
(setq auto-save-default t)
(setq make-backup-files nil)             ; stop creating backup~ files
(setq create-lockfiles nil)              ; stop creating .# files

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                     (time-subtract after-init-time before-init-time))) gcs-done)))

(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-fringe-mode 10)
  (display-time-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1)

  :config
  (setq use-file-dialog nil)
  (setq use-dialog-box t)
  (setq visible-bell t)
  ;; (setq inhibit-splash-screen t)
  ;; (setq inibit-startup-message t)
  ;; (setq inhibit-scratch-message t)
  (setq initial-scratch-message "")

  (fset 'yes-or-no-p 'y-or-n-p)

  (unless (equal "Batery status not available" (battery))
    (display-battery-mode 1))

  ;; Disable the pair of key bindings that involve z minimise the Emacs frame.
  ;; Disable the 'hello' file
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-h h" . nil)))

(toggle-frame-fullscreen)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable for some modes
(dolist (mode '(org-mode-hook
                shell-mode-hook
                eshell-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package modus-vivendi-theme
  :ensure t)

(use-package modus-operandi-theme
  :ensure t)

(use-package all-the-icons
  :ensure t)

(load-theme 'modus-vivendi t)

(defvar wmad/default-font-size 100)

(set-face-attribute 'default nil :font "Roboto Mono Light" :height wmad/default-font-size)

;; Set the fixed pitch face
;;(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height wmad/default-font-size)
;;(set-face-attribute 'fixed-pitch nil :font "Roboto Mono Light" :height wmad/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height wmad/default-font-size)
;;(set-face-attribute 'fixed-pitch nil :font "Noto Sans Italic" :height wmad/default-font-size)

;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height wmad/default-font-size :weight 'regular)

(defun wmad/upload-to-netsuite ()
  "Send buffer to Netsuite."
  (interactive)
  (message (shell-command-to-string (concat "ns-upload" " " (buffer-file-name)))))

(defun wmad/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun wmad/duplicate-line ()
  (interactive)
   (let* ((cursor-column (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column cursor-column)))

(defun wmad/transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

(defun wmad/open-init-file ()
  "Open the ORG init file."
  (interactive)
  (find-file "~/.emacs.d/emacs-init.org"))

(defun wmad/open-journal ()
  "Open the journal file."
  (interactive)
  (find-file "/run/media/wmadruga/3A3D-979D/2nd_brain/journal.org"))

(defun wmad/open-todo ()
  "Open the TODO file."
  (interactive)
  (find-file "/run/media/wmadruga/3A3D-979D/2nd_brain/todo.org"))

(use-package bufler
  :ensure t
  :bind (("C-x b" . bufler)))

(use-package no-littering
  :ensure t
  :config
  (require 'recentf)

  (defvar recentf-exclude)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package recentf
  :config
  (setq recentf-max-saved-items 5000)
  (recentf-mode t))

(global-unset-key (kbd "C-SPC"))

(use-package general
  :ensure t
  :config
  (general-create-definer wmad/leader-keys
    :prefix "C-SPC"
    :global-prefix "C-SPC"))

(use-package amx
  :ensure t
  :config (amx-mode))

(use-package undo-fu
  :ensure t
  ;;    :config (global-undo-tree-mode -1)
  )

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package diminish
  :ensure t
  :after use-package)

(use-package try
  :ensure t)

(use-package restart-emacs
  :ensure t )

(use-package helpful
  :ensure t)

(use-package switch-window
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items
        '((recents . 5)
          (projects . 5)
          (bookmarks . 5)
          (agenda . 20)))
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))
  (dashboard-setup-startup-hook))

(use-package company
  :ensure t
  :after lsp-mode
  :bind (:map company-active-map
              ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ido))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (or (file-directory-p "~/src") (file-directory-p "~/git"))
    (setq projectile-project-search-path '("~/src" "~/git")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package ag
  :ensure t)

(use-package ripgrep
  :ensure t)

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package smartparens
  :ensure t)

(use-package origami
  :ensure t)
(global-origami-mode)

(use-package indent-guide
  :ensure t
  :hook (prog-mode-hook . indent-guide-mode))

(setq-default indent-tabs-mode nil
              fill-column 140
              tab-width 2)

(defun wmad/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package company-lsp
  :ensure t)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(add-hook 'prog-mode-hook 'flyspell-prog-mode) ;; spell Check

(use-package restclient
  :ensure t)

(use-package dumb-jump
  :ensure t)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(use-package js2-mode
  :ensure t)

(use-package js2-refactor
  :ensure t
  :config (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package eglot
  :ensure t)

(setq js-indent-level 2)

(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook 'lsp-deferred)
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'js2-mode-hook 'eglot-ensure)

;; Flycheck configs

;; disable jshint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; enable eslint
(flycheck-add-mode 'javascript-eslint 'js2-mode)

(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package clojure-snippets
  :ensure t)

(use-package clj-refactor
  :ensure t)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(use-package suggest
  :ensure t)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package slime
  :ensure t
    :config
  (setq inferior-lisp-program "sbcl")
  (setq scheme-program-name "scheme"))

;; try out sly: https://github.com/joaotavora/sly 
;; (use-package sly
;;   :ensure t)

(load-file "~/src/netsuite-mode.el/netsuite.el")
(add-to-list 'auto-mode-alist '("\\.js\\'" . netsuite-mode))

(defun wmad/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (diminish org-indent-mode))

(defun wmad/org-font-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (dolist (face '((org-level-1 . 1.7)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Roboto Mono Light" :weight 'regular :height (cdr face)))

  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun wmad/org-mode-visual-fill ())
  ;; (setq visual-fill-column-width 200
  ;; visual-fill-column-center-text t)
  ;; (visual-fill-column-mode 1))

(use-package org
  :hook (org-mode-hook . wmad/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)
  (wmad/org-font-setup))

(use-package org-superstar
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (let ((org-super-agenda-groups
         '((:auto-group t)))))
  (setq org-agenda-window-setup 'current-window))

(use-package visual-fill-column
  :defer t
  :hook (org-mode-hook . wmad/org-mode-visual-fill))

(setq org-log-into-drawer t)
(setq org-agenda-files "~/.emacs.d/elisp/agenda-files.el")

(require 'org-habit)

(add-to-list 'org-modules 'org-habit)

(setq org-habit-graph-column 60)

(setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "STRT(s)"  "WAIT(w)"  "|" "DONE(d!)")))

(setq org-todo-keyword-faces
  '(("TODO" . (:foreground "orange red" :weight bold))
    ("NEXT" . (:foreground "yellow" :weight bold))
    ("STRT" . (:foreground "green" :weight bold))
    ("WAIT" . (:foreground "MediumPurple3" :weight bold))
    ("DONE" . (:foreground "blue" :weight bold))))

(defvar +org-capture-journal-file "/run/media/wmadruga/3A3D-979D/2nd_brain/journal.org")

(setq org-capture-templates
      '(("j" "Journal" entry
   (file+olp+datetree +org-capture-journal-file)
   "* %U %?\n%i\n%a" :prepend t)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))

(use-package window
  :init
  (setq display-buffer-alist
        '(
          ("^\\(\\*Bufler.*\\|\\*Help.*\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.35)
           (side . right)
           (slot . 1))

          ("^\\(\\*e?shell\\|vterm\\|*HTTP.*\\|*Async.*\\|*ag.*\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.15)
           (side . bottom)
           (slot . 0))))

  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  :hook ((help-mode-hook . visual-line-mode)
         (custom-mode-hook . visual-line-mode)))

(use-package dired
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv")
                                ("mp3" . "mpv")
                                ("pdf" . "acroread")
                                )))

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-refresh-on-projectile-switch t)
  (setq dired-sidebar-should-follow-file t)
  (setq dired-sidebar-one-instance-p t))

;; https://github.com/crocket/dired-single/tree/98c2102429fcac6fbfdba9198c126eb1b3dcc4e5
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

(global-set-key (kbd "C-x o")     'switch-window)
(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>")    'windmove-up)
(global-set-key (kbd "s-<down>")  'windmove-down)

(global-set-key (kbd "C-h f")   #'helpful-callable)
(global-set-key (kbd "C-h v")   #'helpful-variable)
(global-set-key (kbd "C-h k")   #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F")   #'helpful-function)
(global-set-key (kbd "C-h C")   #'helpful-command)

(wmad/leader-keys
  "e"   '(wmad/open-init-file :which-key "Open init file")
  "j"   '(wmad/open-journal :which-key "Open journal file")
  "T"   '(wmad/open-todo :which-key "Open todo file")
  "k"   '(kill-buffer :which-key "Kill buffer")
  "SPC" '(projectile-find-file :which-key "Project Find File")
  "R"   '(restart-emacs :which-key "Restart Emacs")
  "v"   '(vterm :which-key "Terminal"))

(wmad/leader-keys
  "d"     '(:ignore t :which-key "Dired")
  "dd"    '(dired-hide-dotfiles-mode :which-key "Hide dotfiles")
  "dj"    '(dired-jump :which-key "Jump")
  "ds"    '(dired-sidebar-show-sidebar :which-key "Show sidebar")
  "dh"    '(dired-sidebar-hide-sidebar :which-key "Hide sidebar")
  "d RET" '(dired-single-buffer :which-key "Single buffer"))

(wmad/leader-keys
  "o"  '(:ignore t :which-key "Org-Mode")
  "oa" '(org-agenda :which-key "Agenda")
  "oc" '(org-capture :which-key "Capture")
  "ol" '(org-insert-link :which-key "Insert Link")
  "oo" '(org-open-at-point :which-key "Open Link"))

(wmad/leader-keys
  "p"  '(:ignore t :which-key "Project")
  "pc" '(projectile-command-map :which-key "All commands")
  "pf" '(projectile-find-file :which-key "Find File")
  "pp" '(projectile-switch-project :which-key "Switch Project")
  "pk" '(projectile-kill-buffers :which-key "Kill Buffers")
  "ps" '(projectile-ag :which-key "Silver Search")
  "pS" '(projectile-ripgrep :which-key "Ripgrep Search"))

(wmad/leader-keys
  "G"  '(:ignore t :which-key "Magit")
  "Gs" '(magit-status :which-key "Status")
  "Gb" '(magit-blame :which-key "Blame")
  "Gl" '(magit-log :which-key "Log")
  "Gf" '(magit-fetch :which-key "Fetch")
  "G <down>" '(magit-pull :which-key "Pull")
  "G <up>" '(magit-push :which-key "Push"))

(wmad/leader-keys
  "t"  '(:ignore t :which-key "Toggle")
  "td" '(dired-sidebar-toggle-sidebar :which-key "dired sidebar")
  "tf" '(toggle-frame-fullscreen :which-key "fullscreen")
  "th" '(load-theme :which-key "choose theme")
  "tm" '(menu-bar-mode :which-key "menu bar")
  "to" '(global-origami-mode :which-key "origami")
  "tt" '(bufler-tabs-mode :which-key "bufler tab bar"))

(wmad/leader-keys
  "n"  '(:ignore t :which-key "Netsuite")
  "nc" '(netsuite/create-project :which-key "Create Project")
  "nd" '(netsuite/deploy :which-key "Deploy Project")
  "nu" '(netsuite/upload-buffer :which-key "Upload buffer"))

(wmad/leader-keys
  "w"  '(:ignore t :which-key "Window")
  "wt" '(wmad/transpose-windows :which-key "Transpose")
  "wo" '(switch-window :which-key "Switch")
  "w-" '(split-window-below :which-key "Split below")
  "w=" '(split-window-right :which-key "Split right")
  "w0" '(delete-window :which-key "Delete this")
  "w1" '(delete-other-windows :which-key "Delete others")
  "w5" '(delete-frame :which-key "Delete frame")
  "w_" '(balance-windows :which-key "Balance")
  "wq" '(window-toggle-side-windows :which-key "Toggle Side windows"))

(general-define-key
 "C-c <down>" 'wmad/duplicate-line
 "C-c d" 'delete-trailing-whitespace)

(wmad/leader-keys
  "b"         '(:ignore t :which-key "Buffer")
  "bb"        '(bufler :which-key "Buffer Window")
  "bs"        '(bufler-switch-buffer :which-key "Switch Buffer")
  "b <right>" '(next-buffer :which-key "Next")
  "b <left>"  '(previous-buffer :which-key "Previous"))

(wmad/leader-keys
  "z"  '(:ignore t :which-key "Origami")
  "za" '(origami-toggle-node :which-key "Toggle node")
  "zo" '(origami-open-node :which-key "Open")
  "zc" '(origami-close-node :which-key "Close"))

(wmad/leader-keys
  "g"  '(:ignore t :which-key "Go to...")
  "gd" '(dumb-jump-go :which-key "go dumb")
  "gb" '(xref-pop-marker-stack :which-key "go back"))
