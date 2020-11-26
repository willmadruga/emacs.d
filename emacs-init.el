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
  :hook ((kill-emacs-hook . prot/rebuild-emacs-init)
         (kill-emacs-hook . package-quickstart-refresh)))

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
  (setq use-dialog-box t)               ;; only for mouse events
  (setq visible-bell t)                 ;; set up the visible bell
  (setq inhibit-splash-screen t)
  (setq inibit-startup-message t)
  (setq inhibit-scratch-message t)

  (fset 'yes-or-no-p 'y-or-n-p)

  (unless (equal "Batery status not available" (battery))
    (display-battery-mode 1))

  ;; Disable the pair of key bindings that involve z minimise the Emacs frame. 
  ;; Disable the 'hello' file
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-h h" . nil)))

;(if (eq initial-window-system 'x)
; (toggle-frame-maximized)
(toggle-frame-fullscreen)
;)

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

;; (use-package doom-themes
;;   :ensure t
;;   :init (load-theme 'doom-dracula t))

;; You will most likely need to adjust this font size for your system!
(defvar wmad/default-font-size 100)

;; Font Configuration ----------------------------------------------------------

(set-face-attribute 'default nil :font "Fira Code Retina" :height wmad/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'regular)

(defun wmad/upload-to-netsuite ()
  "Send buffer to Netsuite."
  (interactive)
  (message (shell-command-to-string (concat "ns-upload" " " (buffer-file-name)))))
  ;;(async-shell-command (concat "ns-upload" " " (buffer-file-name))))

(defun wmad/sdfcli ()
  "Execute async shell command: sdfcli"
  (interactive)
  (async-shell-command (concat "sdfcli deploy -sw -np -authid " wmad-netsuite-sdfcli-authid)))

;; implement sdfcli project switching using buffer name to discover project root

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

;; TODO I can apply the same concept as duplicate line but instead of a line I should yank a marked region.
;; think that should do...

(defun wmad/duplicate-region ()
  (interactive)
   (let* ((cursor-column (current-column)))
     ;; kill region... get start and end of mark.
     ;;(yank)
     ;;(newline)
     ;;(yank)
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

(defun prot/copy-line-or-region (&optional arg)
    "Kill-save the current line or active region.
With \\[universal-argument] duplicate the target instead.  When
region is active, also apply context-aware indentation while
duplicating."
    (interactive "P")
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (pbol (point-at-bol))
           (peol (point-at-eol))
           (indent (if (eq (or rbeg rend) pbol) nil arg)))
      (if arg
          (progn
            (if (use-region-p)
                (progn
                  (copy-region-as-kill rbeg rend)
                  (when (eq (point) rbeg)
                    (exchange-point-and-mark))
                  (prot/new-line-below indent))
              (copy-region-as-kill pbol peol)
              (prot/new-line-below))
            (yank))
        (copy-region-as-kill pbol peol)
        (message "Current line copied"))))

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
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
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
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

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

(use-package suggest
  :ensure t)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(defun wmad/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

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
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))

(defun wmad/org-mode-visual-fill ()
  (setq visual-fill-column-width 200
        visual-fill-column-center-text t))

(use-package org
  :hook (org-mode-hook . wmad/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  (wmad/org-font-setup))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode-hook . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  :hook (org-mode-hook . wmad/org-mode-visual-fill))

(setq org-log-into-drawer t)
(setq org-agenda-files "~/.emacs.d/elisp/agenda-files.el")

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (let ((org-super-agenda-groups
         '((:auto-group t))))
    (org-agenda-list)))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "STRT(s)"  "WAIT(w)"  "|" "DONE(d!)")))

(defvar +org-capture-journal-file "/run/media/wmadruga/3A3D-979D/2nd_brain/journal.org")

(setq org-capture-templates
      '(("j" "Journal" entry
	 (file+olp+datetree +org-capture-journal-file)
	 "* %U %?\n%i\n%a" :prepend t)))

(use-package window
:init
  (setq display-buffer-alist
        '(

          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|*Messages.*\\|Flymake\\|Output\\|*Completions.*\\)\\*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))

          ("^\\(\\*e?shell\\|vterm\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.50)
           (side . bottom)
           (slot . 1))

          ("\\*\\(*HTTP.*\\|*Async.*\\|\\*Help.*\\)\\*"
           (display-buffer-in-side-window)
           (window-width . 0.25)       ; See the :hook
           (side . right)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))))

  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)

  ;; Note that the the syntax for `use-package' hooks is controlled by
  ;; the `use-package-hook-name-suffix' variable.  The "-hook" suffix is
  ;; not an error of mine.
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



;; (global-set-key (kbd "M-x")     #'helm-M-x)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
;; (global-set-key (kbd "M-i")     #'helm-semantic-or-imenu)

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
  "e"   'wmad/open-init-file
  "j"   'wmad/open-journal
  "k"   'kill-buffer
  "SPC" 'projectile-find-file
  "R"   'restart-emacs
  "T"   'wmad/open-todo
  "v"   'vterm)

(wmad/leader-keys
  "d"     '(:ignore t :which-key "Dired")
  "dd"    'dired-hide-dotfiles-mode
  "dj"    'dired-jump
  "ds"    'dired-sidebar-show-sidebar
  "dh"    'dired-sidebar-hide-sidebar
  "dt"    'dired-sidebar-toggle-sidebar
  "d RET" 'dired-single-buffer)

(wmad/leader-keys
  "o"  '(:ignore t :which-key "Org-Mode")
  "oa" 'org-agenda
  "oc" 'org-capture
  "ol" 'org-insert-link
  "oo" 'org-open-at-point)

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
  "tt" '(tab-bar-mode :which-key "tab bar"))

(wmad/leader-keys
  "n"  '(:ignore t :which-key "Netsuite")
  "nu" '(wmad/upload-to-netsuite :which-key "Upload buffer")
  "ns" '(wmad/sdfcli :which-key "Deploy"))

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
 "C-c <down>" 'wmad/duplicate-line)

(wmad/leader-keys
  "b"         '(:ignore t :which-key "Buffer")
  "bb"        '(bufler-switch-buffer :which-key "Switch Buffer")
  "bw"        '(bufler :which-key "Buffer Window")
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
