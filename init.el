;;; init.el --- Initialization File ;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; - A few years of configuration, mostly copied from others but some original work too.
;;
;; - Switched back and forth between different configuration approaches, think I am settling
;;   with this one, we'll see.
;;
;; - Don't think I will modularize things again, let me handle this one single file and have
;;   separate code by functions.
;;
;; - with-package macro learned and taken from:
;;   Declarative Package Configuration In 5 Lines of Emacs Lisp
;;   https://cosine.blue/emacs-with-package.html
;;
;; - Some configuration taken from Spartan Emacs
;;   https://github.com/grandfoobah/spartan-emacs
;;   https://github.com/wmadruga/spartan-emacs
;;
;; - Some performance related notes
;;   https://emacs-lsp.github.io/lsp-mode/page/performance/
;;   https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
;;
;; - Packages are installed at the very end, requires restart after first setup
;;
;; - There is a fundamental flaw when freshly installed.  Not in the mood to spend more time on it.
;;   I can follow up later on, it's likely that I will install this once and grow the configuration
;;   from there, so no worries.  Let me document what I need to do whenever this is freshly installed:
;;   1. Open Emacs first time and let it finish downloading packages.
;;   2. Configuration will be applied after first restart.
;;   3. Manually create the 'var' folder if Emacs fails to quit.
;;   4. user-emacs-directory is not clean as supposed to (using no-littering)
;;   5. Manually clean it up by removing the transient folder and the history file.
;;   6. That's it, from now on files and folders are created inside 'var' and 'etc'

;;; Code:

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ____                               _   _
;; / ___| _   _ _ __  _ __   ___  _ __| |_(_)_ __   __ _
;; \___ \| | | | '_ \| '_ \ / _ \| '__| __| | '_ \ / _` |
;;  ___) | |_| | |_) | |_) | (_) | |  | |_| | | | | (_| |
;; |____/ \__,_| .__/| .__/ \___/|_|   \__|_|_| |_|\__, |
;;             |_|   |_|                           |___/
;;  _____                 _   _
;; |  ___|   _ _ __   ___| |_(_) ___  _ __  ___
;; | |_ | | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; |  _|| |_| | | | | (__| |_| | (_) | | | \__ \
;; |_|   \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-package (package &rest body)
  "Add PACKAGE to 'package-selected-packages'.
Then attempt to ‘require’ PACKAGE and, if successful, evaluate BODY."
  (declare (indent 1))
  `(and (add-to-list 'package-selected-packages ,package)
        (require ,package nil 'noerror)
        (progn ,@body)))

(defun immortal-scratch ()
  "Enable immortal scratch buffer."
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil)
    t))

(defun save-persistent-scratch ()
  "Save the contents of *scratch*."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max)
                  (concat user-emacs-directory "/var/scratch"))))

(defun load-persistent-scratch ()
  "Reload the scratch buffer."
  (let ((scratch-file (concat user-emacs-directory "/var/scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file)))))

;; Interactive Supporting Functions
(defun wmadruga/eslint-fix-buffer-file ()
  "Use eslint to fix buffer file."
  (interactive)
  (async-shell-command (concat "eslint --fix " (buffer-file-name))))

(defun wmadruga/duplicate-line ()
  "Duplicates line at point."
  (interactive)
  (let* ((cursor-column (current-column)))
    (move-beginning-of-line 1)
    (kill-line) (yank) (newline) (yank)
    (move-to-column cursor-column)))

(defun wmadruga/update-my-orgids ()
  "Forcefully update orgids for files I care about."
  (interactive)
  (org-id-update-id-locations (directory-files "~/src/2nd_brain/brain")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ____                           _
;;  / ___| ___ _ __   ___ _ __ __ _| |
;; | |  _ / _ \ '_ \ / _ \ '__/ _` | |
;; | |_| |  __/ | | |  __/ | | (_| | |
;;  \____|\___|_| |_|\___|_|  \__,_|_|
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun daemon-init ()
  "Daemon initialization."
  (require 'server)
  (or (server-running-p)
      (server-start)))

(defun performance-config ()
  "Performance configuration."
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024))

  (if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
      (setq comp-deferred-compilation t
            package-native-compile t)
    (message "Native complation is *not* available, lsp performance will suffer..."))

  (unless (functionp 'json-serialize)
    (message "Native JSON is *not* available, lsp performance will suffer..."))

  (setq warning-suppress-types '((comp)))
  (setq read-process-output-max (* 1024 1024)))

(defun initialization-config ()
  "Initialization Config."

  ;; Setup no-literring for a cleaner .emacs.d directory.
  (with-package 'no-littering
    (require 'recentf)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (add-to-list 'recentf-exclude "~/.emacs.d/elpa")
    (recentf-mode 1)
    (setq auto-save-file-name-transforms
	        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
    (setq backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backups")))))

  (setq inhibit-startup-screen nil
        initial-major-mode 'org-mode
        load-prefer-newer t
        use-dialog-box t
        undo-limit 80000000
        make-backup-files nil
        create-lockfiles nil
        recentf-max-saved-items 50
        tab-always-indent 'complete
        shell-file-name "/usr/bin/zsh"
        explicit-shell-file-name "/usr/bin/zsh")

  (setq-default frame-resize-pixelwise t
                indent-tabs-mode nil
                tab-width 2
                fill-column 140
                gnutls-verify-error t
                gnutls-min-prime-bits 2048
                password-cache-expiry nil
                mouse-yank-at-point t
                save-interprogram-paste-before-kill t
                apropos-do-all t
                require-final-newline t
                ediff-window-setup-function 'ediff-setup-windows-plain
                vc-follow-symlinks t
                ring-bell-function 'ignore
                browse-url-browser-function 'eww-browse-url)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'ff 'find-name-dired)
  (defalias 'rg 'rgrep)
  (defalias 'ed 'ediff)
  (defalias 'edb 'ediff-buffers)

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  (show-paren-mode 1)
  (global-eldoc-mode 1)
  (winner-mode 1)
  (electric-pair-mode 1)
  (delete-selection-mode 1)
  (savehist-mode 1)
  (add-hook 'prog-mode-hook 'goto-address-mode) ; make comment urls clickable
  (global-display-line-numbers-mode t)
  (put 'narrow-to-region 'disabled nil)

  (menu-bar-mode -1)
  (tool-bar-mode -1)

  (and (display-graphic-p)
       (progn
         (scroll-bar-mode -1)
         (fringe-mode -1)))

  (dolist (mode '(org-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  vterm-mode-hook
                  term-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (require 'subr-x)
  (or (getenv "EDITOR")
      (progn
        (setenv "EDITOR" "emacsclient")
        (setenv "VISUAL" (getenv "EDITOR"))))

  (or (getenv "PAGER")
      (setenv "PAGER" "cat"))

  ;; 'PATH' modifications
  (dolist
      (item '(
              "~/bin"
              "~/.local/bin"
              "~/.nix-profile/bin"
              "~/.deno/bin"
              "~/.config/guix/usr/local/sbin"
              "~/devenv/apache-maven-3.6.1/bin"
              "~/.npm-packages/bin"
              ))
    (add-to-list 'exec-path item))

  (setenv "PATH" (string-trim-right (string-join exec-path ":") ":$"))

  (setenv "SHELL" shell-file-name)
  ;; 	(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

  ;; for terminal Emacs only (emacs -nw)
  (or (display-graphic-p)
      (progn
        (with-package 'clipetty
	        (global-clipetty-mode +1))
        (xterm-mouse-mode 1)
        (global-set-key (kbd "C-x ;") 'comment-line) ; "C-x C-;" is interpreted this way in some terminals
        )))

(defun bindings-config ()
  "General bidings configuration."

  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "M-m"))

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-/") 'hippie-expand)

  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  (global-set-key (kbd "s-<right>")   'windmove-right)
  (global-set-key (kbd "s-<left>")    'windmove-left)
  (global-set-key (kbd "s-<up>")      'windmove-up)
  (global-set-key (kbd "s-<down>")    'windmove-down)

  (global-set-key (kbd "C-c o c") 'org-capture)

  ;; interactive supporting functions
  (global-set-key (kbd "C-c <down>") 'wmadruga/duplicate-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ____            _
;; |  _ \ __ _  ___| | ____ _  __ _  ___
;; | |_) / _` |/ __| |/ / _` |/ _` |/ _ \
;; |  __/ (_| | (__|   < (_| | (_| |  __/
;; |_|   \__,_|\___|_|\_\__,_|\__, |\___|
;;                            |___/
;;   ____             __ _
;;  / ___|___  _ __  / _(_) __ _
;; | |   / _ \| '_ \| |_| |/ _` |
;; | |__| (_) | | | |  _| | (_| |
;;  \____\___/|_| |_|_| |_|\__, |
;;                         |___/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-system-init ()
  "Package system setup and initialization.  Prefer GNU ELPA > MELPA (fallback)."

  (setq package-enable-at-startup nil)
  (setq package-archives
        '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
          ("MELPA"        . "https://melpa.org/packages/"))
        package-archive-priorities
        '(("GNU ELPA"     . 10)
          ("MELPA" . 5)))
  (package-initialize))

(defun package-bootstrap ()
  "Install missing packages."

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (package package-selected-packages)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___
;;  / _ \ _ __ __ _
;; | | | | '__/ _` |
;; | |_| | | | (_| |
;;  \___/|_|  \__, |
;;            |___/
;;  ____      _       _           _
;; |  _ \ ___| | __ _| |_ ___  __| |
;; | |_) / _ \ |/ _` | __/ _ \/ _` |
;; |  _ <  __/ | (_| | ||  __/ (_| |
;; |_| \_\___|_|\__,_|\__\___|\__,_|
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-config ()
  "Org-Mode related config."

  (if (file-exists-p "~/src/2nd_brain/")
      (setq org-directory "~/src/2nd_brain/")
    (setq org-directory "~/"))

  (setq
   org-adapt-indentation nil
	 org-ellipsis " ▼ "
	 org-hide-emphasis-markers t
	 org-todo-keywords
	 '((sequence
	    "TODO(t)"
	    "NEXT(n)"
	    "STRT(s)"
	    "WAIT(x)"
	    "|"
	    "DONE(d)"
	    "CANCELLED(c)")))

  ;; org capture
  (if (file-exists-p "~/src/2nd_brain/journal.org")
      (defvar +org-capture-journal-file "~/src/2nd_brain/journal.org")
    (defvar +org-capture-journal-file "~/journal.org"))

  (setq org-capture-templates
	      '(("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %?\n%i\n%a" :prepend t :jump-to-captured t)))

  ;; org-agenda
  (if (file-exists-p "~/src/2nd_brain/brain")
      (setq org-agenda-files '("~/src/2nd_brain/brain/TODO-LIST.org" "~/src/2nd_brain/brain/Finances.org")))

  (setq
   org-agenda-start-day "0d"
   org-agenda-span 5
   org-agenda-include-diary t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-use-time-grid t
   appt-display-duration 60)

  (with-package 'org-brain

    (if (file-exists-p "~/src/2nd_brain/brain")
        (setq org-brain-path "~/src/2nd_brain/brain"
              org-id-locations-file "~/src/2nd_brain/brain/.orgids"))

    (setq
     org-id-locations-file-relative t
     org-brain-visualize-default-choices 'all
     org-brain-title-max-length 12
     org-brain-include-file-entries nil
     org-brain-file-entries-use-title nil)

    (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
    (push '("b" "Brain" plain (function org-brain-goto-end)
	          "* %i%?" :empty-lines 1)
	        org-capture-templates)

    (global-set-key (kbd "C-c o b") 'org-brain-goto)
    (global-set-key (kbd "C-c o a") 'org-brain-agenda))

  (with-package 'org-superstar)

  ;; think this can be improved, right?
  (with-package 'org-bullets
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ___ ____  _____    ___
;; |_ _|  _ \| ____|  ( _ )
;;  | || | | |  _|    / _ \/\
;;  | || |_| | |___  | (_>  <
;; |___|____/|_____|  \___/\/

;;  _____      _
;; | ____|_  _| |_ _ __ __ _ ___
;; |  _| \ \/ / __| '__/ _` / __|
;; | |___ >  <| |_| | | (_| \__ \
;; |_____/_/\_\\__|_|  \__,_|___/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ide-config ()
  "IDE features."

  (line-number-mode nil)
  (column-number-mode -1)

  (with-package 'magit
    (defalias 'git 'magit))

  (with-package 'dumb-jump
    (setq dumb-jump-default-project user-emacs-directory
	        dumb-jump-prefer-searcher 'rg
	        dumb-jump-aggressive nil
	        dumb-jump-selector 'ivy)

    (global-set-key (kbd "C-c g") 'dumb-jump-go)
    (global-set-key (kbd "C-c b") 'dumb-jump-back))

  (with-package 'ws-butler
    (add-hook 'text-mode-hook 'ws-butler-mode)
    (add-hook 'prog-mode-hook 'ws-butler-mode))

  (with-package 'rainbow-delimiters
    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

  (with-package 'undo-fu
    (global-set-key (kbd "C-z") 'undo-fu-only-undo)
    (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

  (with-package 'origami
    (global-origami-mode)
    (global-set-key (kbd "C-c z") 'origami-toggle-node))

  (with-package 'ibuffer-vc
    (add-hook 'ibuffer-hook
	            (lambda ()
		            (ibuffer-vc-set-filter-groups-by-vc-root)
		            (unless (eq ibuffer-sorting-mode 'alphabetic)
		              (ibuffer-do-sort-by-alphabetic)))))

  (with-package 'dired-single
    (require 'dired)
    (define-key dired-mode-map [remap dired-find-file]
      'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
      'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory]
      'dired-single-up-directory))

  (with-package 'company
    (setq company-minimum-prefix-length 2
	        company-tooltip-limit 14
	        company-tooltip-align-annotations t
	        company-require-match 'never
	        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
	        company-frontends '(company-pseudo-tooltip-frontend
		                          company-echo-metadata-frontend)
	        company-backends '(company-capf)
	        company-auto-complete nil
	        company-auto-complete-chars nil
	        company-dabbrev-other-buffers nil
	        company-dabbrev-ignore-case nil
	        company-dabbrev-downcase nil)

    (add-hook 'after-init-hook 'global-company-mode))

  ;; having a couple of issues, let's have it disabled for awhile, see if I will miss it...
  ;; (with-package 'company-box
  ;;   (add-hook 'company-mode-hook 'company-box-mode))

  (with-package 'browse-kill-ring
    (global-set-key (kbd "M-y") 'browse-kill-ring))

  (with-package 'swiper
    (setq swiper-action-recenter t)
    (global-set-key (kbd "C-s") 'swiper)))

(defun js-ide-config ()
  "Configure javascript supporting IDE tools."

  ;; ONLY if node.js is installed.
  ;; (and (executable-find "node")

  (with-package 'eglot
    (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))

    (define-key eglot-mode-map (kbd "M-m dd") 'eldoc)
    (define-key eglot-mode-map (kbd "M-,") 'eglot-rename)
    (define-key eglot-mode-map (kbd "M-=") 'eglot-format)
    (define-key eglot-mode-map (kbd "M-?") 'xref-find-references)
    (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
    (define-key eglot-mode-map (kbd "M-/") 'completion-at-point))

  (with-package 'js2-refactor)
  (with-package 'fill-column-indicator)
  (with-package 'js2-mode
    (setq
     js-chain-indent t
     js2-skip-preprocessor-directives t
     js2-mode-show-parse-errors nil
     js2-mode-show-strict-warnings nil
     js2-strict-trailing-comma-warning nil
     js2-strict-missing-semi-warning nil
     js2-highlight-level 3
     js2-highlight-external-variables t
     js2-idle-timer-delay 0.1)

    (setq-default
     tab-width 2
     indent-tabs-mode nil
     fill-column 140
     js2-basic-offset 2
     js-indent-level 2
     js-indent-first-init nil
     js-indent-align-list-continuation nil)

    ;; Netsuite SDFCLI wrapper - temporary lib I am working on, name is likely to change.
    (cond
     ((file-exists-p "~/src/netsuite-sdf/sdfcli.el")
      (progn
        (load-file "~/src/netsuite-sdf/sdfcli.el"))))

    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook 'flycheck-mode)
    (add-hook 'js2-mode-hook 'js2-refactor-mode)
    (add-hook 'js2-mode-hook 'show-paren-mode)
    (add-hook 'js2-mode-hook 'fci-mode)
    (add-hook 'js2-mode-hook 'eglot-ensure))

  (with-package 'flycheck
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (setq-default flycheck-temp-prefix ".flycheck")
    (setq flycheck-javascript-eslint-executable "~/.npm-packages/bin/eslint")

    ;; config for Javascript
    (setq-default flycheck-disabled-checkers
		              (append flycheck-disabled-checkers
			                    '(javascript-jshint)
			                    '(json-jsonlist)))

    ;; enable eslint
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (setq flycheck-highlighting-mode 'lines)
    (setq-default flycheck-temp-prefix ".flycheck")

    (add-hook 'prog-mode-hook 'global-flycheck-mode)))

(defun extra-packages-config ()
  "Layers config."
  (with-package 'restart-emacs)
  (with-package 'hnreader)
  (with-package 'writefreely)

  (with-package 'olivetti
    (setq olivetti-body-width 165)
    ;; W for writer duh!
    (global-set-key (kbd "C-c w") 'olivetti-mode))

  (with-package 'elpher
    (setq gnutls-verify-error 'nil))

  (with-package 'crux
    (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
    (global-set-key (kbd "C-o") 'crux-smart-open-line)
    (global-set-key (kbd "C-c C-l") 'crux-duplicate-current-line-or-region)
    (global-set-key (kbd "C-c C--") 'crux-kill-whole-line)
    (global-set-key (kbd "C-c ;") 'crux-duplicate-and-comment-current-line-or-region))

  (with-package 'helpful)

  (with-package 'which-key
    (which-key-mode))

  (with-package 'emacs-everywhere
    ;; slightly bigger window
    (setq emacs-everywhere-frame-parameters
          '((name . "emacs-everywhere")
            (width . 80)
            (height . 40)))
    ;; force org-mode loading
    (setq emacs-everywhere-init-hooks
          '(emacs-everywhere-set-frame-name
            emacs-everywhere-set-frame-position
            org-mode
            emacs-everywhere-insert-selection
            emacs-everywhere-remove-trailing-whitespace
            emacs-everywhere-init-spell-check))
    ))

(defun ivy-related-config ()
  "Ivy related configuration to be called last."

  (with-package 'ivy
    (ivy-mode 1)
    (setq ivy-sort-max-size 7500)

    (setq ivy-height 17
	        ivy-wrap t
	        ivy-fixed-height-minibuffer t
	        ivy-read-action-function #'ivy-hydra-read-action
	        ivy-read-action-format-function #'ivy-read-action-format-columns
	        ivy-use-virtual-buffers nil
	        ivy-virtual-abbreviate 'full
	        ivy-on-del-error-function #'ignore
	        ivy-use-selectable-prompt t
	        ivy-initial-inputs-alist nil))

  (with-package 'counsel
    (require 'counsel nil t) ;; load as early as possible
    (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-x C-/") 'counsel-imenu)
    (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
    (global-set-key (kbd "C-c r") 'counsel-recentf))

  (with-package 'prescient
    (prescient-persist-mode 1))

  (with-package 'ivy-prescient
    (ivy-prescient-mode 1))

  ;; Configure ivy-posframe after other packages so I can map them.
  (with-package 'ivy-posframe
    (setq ivy-fixed-height-minibuffer nil
	        ivy-posframe-border-width 10
	        ivy-posframe-parameters
	        `((min-width . 90)
	          (min-height . ,ivy-height)))

    (setq ivy-posframe-display-functions-alist
	        '((swiper                     . ivy-posframe-display-at-window-center)
	          (complete-symbol            . ivy-posframe-display-at-window-center)
	          (counsel-M-x                . ivy-posframe-display-at-window-center)
	          (helpful-callable           . ivy-posframe-display-at-window-center)
	          (helpful-function           . ivy-posframe-display-at-window-center)
	          (helpful-variable           . ivy-posframe-display-at-window-center)
	          (helpful-key                . ivy-posframe-display-at-window-center)
	          (helpful-at-point           . ivy-posframe-display-at-window-center)
	          (helpful-command            . ivy-posframe-display-at-window-center)
	          (counsel-find-file          . ivy-posframe-display-at-window-center)
	          (counsel-recentf            . ivy-posframe-display-at-window-center)
	          (project-switch-project     . ivy-posframe-display-at-window-center)
	          (project-find-file          . ivy-posframe-display-at-window-center)
	          (org-brain-goto             . ivy-posframe-display-at-window-center)
	          (dumb-jump-go               . ivy-posframe-display-at-window-center)
	          (ivy-switch-buffer          . ivy-posframe-display-at-window-center)
	          (counsel-imenu              . ivy-posframe-display-at-window-center)
	          (nil                        . ivy-posframe-display))
	        ivy-posframe-height-alist
	        '((swiper . 20)
	          (dmenu . 20)
	          (t . 10)))

    (ivy-posframe-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _____ _
;; |_   _| |__   ___ _ __ ___   ___
;;   | | | '_ \ / _ \ '_ ` _ \ / _ \
;;   | | | | | |  __/ | | | | |  __/
;;   |_| |_| |_|\___|_| |_| |_|\___|
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun theme-config ()
  "Theme configuration."

  (with-package 'modus-themes

    (setq modus-themes-bold-constructs t
	        modus-themes-mode-line '3d)
    (modus-themes-load-themes)
    (modus-themes-load-vivendi))

  (with-package 'doom-modeline
    (doom-modeline-mode 1)
    (add-hook 'after-init-hook #'doom-modeline-mode))

  (add-hook 'kill-buffer-query-functions 'immortal-scratch)
  (add-hook 'after-init-hook 'load-persistent-scratch)
  (add-hook 'kill-emacs-hook 'save-persistent-scratch)
  (run-with-idle-timer 300 t 'save-persistent-scratch)

  (setq display-buffer-alist

        ;;TODO: always display vterm on a dedicated, reusable window, add a toggling function, bind to a key

        ;; display the window for the async shell command output at the bottom, small height.
        '(("\\.*Async Shell Command.*"
           (display-buffer-at-bottom)
           (window-height . 0.1)
           )))

  (with-package 'dashboard
    (setq dashboard-center-content t)
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner 'logo)
    (setq dashboard-banner-logo-title nil)
    (setq dashboard-page-separator "\n\n")
    (setq dashboard-set-init-info t)
    (setq dashboard-footer-messages '("\"Imagine all the people, living life in peace.\""))
    (setq dashboard-items '((recents  . 10)
			                      (agenda . 20)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ____          _
;;  / ___|   _ ___| |_ ___  _ __ ___
;; | |  | | | / __| __/ _ \| '_ ` _ \
;; | |__| |_| \__ \ || (_) | | | | | |
;;  \____\__,_|___/\__\___/|_| |_| |_|
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custom-config ()
  "Load custom configuration."
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load-file custom-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _   _           _
;; | | | |_   _  __| |_ __ __ _
;; | |_| | | | |/ _` | '__/ _` |
;; |  _  | |_| | (_| | | | (_| |
;; |_| |_|\__, |\__,_|_|  \__,_|
;;        |___/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hydra-config ()
  "Load Hydra configuration."
  (with-package 'hydra
    (load-file "hydra-config.el")))

;;;;;;;;;;;;;;;;;;;;;;;
;;  ___       _ _    ;;
;; |_ _|_ __ (_) |_  ;;
;;  | || '_ \| | __| ;;
;;  | || | | | | |_  ;;
;; |___|_| |_|_|\__| ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(performance-config)
(initialization-config)
(bindings-config)

(ide-config)
(ivy-related-config)
(js-ide-config)
(extra-packages-config)
(org-config)
(custom-config)

(hydra-config)

(theme-config)
(daemon-init)

;; packages are installed at the very end, requires restart after first setup
(package-system-init)
(package-bootstrap)
;;; init.el ends here
