;;; init.el --- Emacs init ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; TODO: Build EMACS 28 with native-compilation:
;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation

;; Garbage collection stuff
;; https://gitlab.com/nathanfurnal/dotemacs/-/blob/master/init.el
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly


;;; Code:

(require 'package)
(require 'warnings)
(require 'recentf)
(require 'shell)
(require 'dired)
(require 'server)

(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'appt)

(setq package-archives
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("GNU ELPA" . 10)
        ("MELPA"    . 5)))

(unless (bound-and-true-p package--initialized)
  (package-initialize))





;; 3RD PARTY PACKAGES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(dolist (pname '(
                 gcmh modus-themes mixed-pitch
                 consult consult-flycheck vertico marginalia orderless
                 ibuffer-vc dired-single which-key crux diminish
                 move-text dumb-jump corfu origami indent-guide rainbow-delimiters
                 org-roam calfw calfw-org
                 projectile magit js2-mode eglot flycheck yasnippet yasnippet-snippets restclient jira-markup-mode
                 hnreader helpful devdocs-browser equake md4rd
                 exwm exwm-edit desktop-environment
                 ))
  (unless (package-installed-p pname)
    (progn
      (package-refresh-contents)
      (package-install pname))))





;; PERFORMANCE TWEAKS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'gcmh)
(setq gcmh-mode 1)
(setq gcmh-idle-delay 5)
(setq gcmh-high-cons-threshold (* 16 1024 1024))

(if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t
          native-comp-deferred-compilation t
          native-comp-async-query-on-exit t
          native-comp-async-jobs-number 6
          native-comp-async-report-warnings-errors nil)
  (message "Native compilation is *not* available, consider enabling it."))

(unless (functionp 'json-serialize)
  (message "Native JSON is *not* available, consider enabling it."))



;; COMMON SETUP  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defalias 'yes-or-no-p 'y-or-n-p)

;; (setq inhibit-startup-screen                           t)
(setq inhibit-default-init                                t)
(setq load-prefer-newer                                   t)
(setq make-backup-files                                 nil)
(setq create-lockfiles                                  nil)
(setq browse-url-browser-function       'browse-url-generic)
(setq browse-url-generic-program              "qutebrowser")
(setq read-process-output-max                 (* 1024 1024))
(setq warning-suppress-types                      '((comp)))
(setq recentf-max-saved-items                            50)
(setq undo-limit                                   80000000)
(setq tab-always-indent                           'complete)
(setq initial-major-mode                          'org-mode)
(setq explicit-shell-file-name               "/usr/bin/zsh")
(setq shell-file-name                        "/usr/bin/zsh")
(setq initial-scratch-message "#+title: Scratch Buffer\n\n")
(setq initial-major-mode                          'org-mode)
(setq winner-dont-bind-my-keys                            t)

(setq-default line-spacing 2)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default save-interprogram-paste-before-kill                  t)
(setq-default gnutls-verify-error                                  t)
(setq-default apropos-do-all                                       t)
(setq-default vc-follow-symlinks                                   t)
(setq-default require-final-newline                                t)
(setq-default frame-resize-pixelwise                               t)
(setq-default mouse-yank-at-point                                  t)
(setq-default password-cache-expiry                              nil)
(setq-default indent-tabs-mode                                   nil)
(setq-default tab-width                                            2)
(setq-default fill-column                                        140)
(setq-default gnutls-min-prime-bits                             2048)
(setq-default ring-bell-function                             'ignore)

(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'mixed-pitch-mode)

(dolist (mode '(org-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(global-display-line-numbers-mode  t)
(add-to-list 'recentf-exclude (concat user-emacs-directory "elpa"))
(put 'narrow-to-region 'disabled nil)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(delete-selection-mode 1)
(global-eldoc-mode     1)
(electric-pair-mode    1)
(winner-mode           1)
(column-number-mode    1)
(show-paren-mode       1)
(savehist-mode         1)
(recentf-mode          1)
(tab-bar-mode          1)
(line-number-mode    nil)

(dolist
    (item '("~/bin"
            "~/.local/bin"
            "~/.nix-profile/bin"
            "~/.deno/bin"
            "~/devenv/apache-maven-3.6.1/bin"
            "~/.npm-packages/bin"
            ))
  (add-to-list 'exec-path item))
(setenv "PATH" (string-trim-right (string-join exec-path ":") ":$"))
(setenv "SHELL" shell-file-name)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; IDE-LIKE CONFIGURATION  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

(require 'projectile)
(autoload 'projectile-project-root "projectile")
(projectile-mode +1)

(require 'indent-guide)
(indent-guide-global-mode)

(require 'magit)
(defalias 'git 'magit)

(require 'ibuffer-vc)
(add-hook 'ibuffer-hook
          (lambda ()
            (require 'ibuf-ext)
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(require 'rainbow-delimiters)
(add-hook 'prog-text-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'dumb-jump)
(setq dumb-jump-default-project user-emacs-directory)
(setq dumb-jump-prefer-searcher 'rg)
(setq dumb-jump-aggressive nil)

(require 'dired-single)
(define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
(define-key dired-mode-map [remap dired-up-directory]                 'dired-single-up-directory)
(define-key dired-mode-map [remap dired-find-file]                    'dired-single-buffer)

(require 'yasnippet)
(yas-global-mode 1)

(require 'consult)
(setq consult-project-root-function #'projectile-project-root)
(setq xref-show-xrefs-function #'consult-xref)
(setq xref-show-definitions-function #'consult-xref)

(defvar my-consult-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-s" #'previous-history-element)
    map))
(consult-customize
 consult-line :keymap my-consult-line-map)

(consult-customize
 consult-ripgrep
 consult-git-grep
 consult-grep
 consult-recent-file
 :preview-key (kbd "M-."))

(require 'vertico)
(vertico-mode)

(require 'marginalia)
(marginalia-mode)

(require 'orderless)
(setq completion-styles '(orderless))

(require 'corfu)
(corfu-global-mode)

(require 'which-key)
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 10000)
(setq which-key-idle-secondary-delay 0.05)
(setq which-key-sort-order 'which-key-key-order-alpha)
(which-key-mode 1)

(require 'diminish)
(diminish 'auto-revert-mode)
(diminish 'which-key-mode)
(diminish 'indent-guide-mode)
(diminish 'gcmh-mode)
(diminish 'eldoc-mode)
(diminish 'flymake-mode)

(diminish 'projectile-mode " Projectile")
;; TODO: diminish "Javascript-IDE"...

;; JAVASCRIPT LANGUAGE CONFIG  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'eglot)
(add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))

(require 'js2-mode)
(setq js-chain-indent t)
(setq js2-skip-preprocessor-directives t)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-trailing-comma-warning nil)
(setq js2-strict-missing-semi-warning nil)
(setq js2-highlight-level 3)
(setq js2-highlight-external-variables t)
(setq js2-idle-timer-delay 0.1)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 140)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default js-indent-first-init nil)
(setq-default js-indent-align-list-continuation nil)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'show-paren-mode)
(add-hook 'js2-mode-hook 'eglot-ensure)

(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)
(setq-default flycheck-temp-prefix ".flycheck")
(setq flycheck-javascript-eslint-executable "~/.npm-packages/bin/eslint")

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)
                      '(json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'js2-mode)
(setq flycheck-highlighting-mode 'lines)
(setq-default flycheck-temp-prefix ".flycheck")

(add-hook 'prog-mode-hook 'global-flycheck-mode)

(load-file (expand-file-name "sdfcli.el" user-emacs-directory))

;; ORG-MODE CONFIG  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq org-return-follows-link t)
(setq org-adapt-indentation t)
(setq org-startup-indented t)
(setq org-startup-folded t)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width '(300))
(setq org-hide-emphasis-markers t)
(setq org-ellipsis " ▼ ")
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "NEXT(n)"
         "STRT(s)"
         "WAIT(x)"
         "|"
         "DONE(d)"
         "CANCELLED(c)")))

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 2)
(setq org-src-window-setup 'current-window)

(setq org-fontify-whole-heading-line t)
(setq org-fontify-done-headline nil)
(setq org-fontify-quote-and-verse-blocks t)
(setq org-timer-display 'both)
(setq org-clock-sound (concat user-emacs-directory "alert.wav"))

;; org-roam
(setq org-roam-v2-ack t)
(setq org-roam-directory "~/src/org-roam")
(setq org-roam-completion-everywhere t)
(setq org-roam-db-gc-threshold most-positive-fixnum)
(org-roam-db-autosync-mode)

(require 'org-roam-dailies)
(setq org-roam-dailies-directory "journal/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

;; org-agenda
(if (file-exists-p "~/src/org-roam")
    (setq org-agenda-files
          '(
            "~/src/org-roam/20210824165029-finances.org"
            "~/src/org-roam/20210824165844-todo.org")))

(setq org-agenda-start-day "0d")
(setq org-agenda-span 5)
(setq org-agenda-include-diary t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-use-time-grid t)
(setq appt-display-duration 60)

(setq org-id-locations-file-relative t)

;; MISCELANEA CONFIG  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(add-hook 'kill-buffer-query-functions
          (lambda ()
            (if (eq (current-buffer) (get-buffer "*scratch*"))
                (progn (bury-buffer)
                       nil))
            t))

(load-file (expand-file-name "password-store.el" user-emacs-directory))

;; UI/THEME CONFIG  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'modus-themes)
(setq modus-themes-bold-constructs t
      modus-themes-mode-line '3d)
(modus-themes-load-themes)
(modus-themes-load-vivendi)

(setq display-buffer-alist
      ;; display the window for the async shell command output at the bottom, small height.
      '(("\\.*Async Shell Command.*"
         (display-buffer-at-bottom)
         (window-height . 0.1)
         )))

;; https://www.gonsie.com/blorg/modeline.html
(setq-default mode-line-format
              (list
               ;; day and time
               '(:eval (propertize (format-time-string " %b %d %H:%M ")
                                   'face 'font-lock-builtin-face))

               '(:eval (propertize (substring vc-mode 5)
                                   'face 'font-lock-constant-face))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b "
                                   'face
                                   (let ((face (buffer-modified-p)))
                                     (if face 'font-lock-warning-face
                                       'font-lock-type-face))
                                   'help-echo (buffer-file-name)))

               ;; line and column
               ;; '%02' to set to 2 chars at least; prevents flickering
               " ("
               (propertize "%02l" 'face 'font-lock-keyword-face) ","
               (propertize "%02c" 'face 'font-lock-keyword-face)
               ") "

               ;; relative position
               "[" (propertize "%p" 'face 'font-lock-constant-face) "] "

               ;; mode-line-modes

               mode-line-misc-info

               ;; spaces to align right
               '(:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ (string-width org-mode-line-string) (+ 3 (string-width mode-name)))
                                              )))))

               (propertize org-mode-line-string 'face '(:foreground "#5DD8FF"))

               ;; the current major mode
               (propertize " %m " 'face 'font-lock-string-face)
               ))

(set-face-attribute 'mode-line nil
                    :background "#353644"
                    :foreground "white"
                    :box '(:line-width 8 :color "#353644")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#565063"
                    :foreground "white"
                    :box '(:line-width 8 :color "#565063")
                    :overline nil
                    :underline nil)

(set-face-attribute 'default        nil :font "DejaVu Sans Mono" :height 100)
(set-face-attribute 'fixed-pitch    nil :font "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans")


;; BINDINGS CONFIG  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; TODO: do I want to use "hydra.el" again? I liked using the rectangle functionality...
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-S-z"))

(move-text-default-bindings)

(global-set-key (kbd "C-c c a") 'consult-apropos)
(global-set-key (kbd "C-c c b") 'consult-buffer)
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

(global-set-key (kbd "C-c <home>")    'org-timer-set-timer)
(global-set-key (kbd "C-c <end>")     'org-timer-pause-or-continue)

(global-set-key (kbd "C-c <up>")      'windmove-up)
(global-set-key (kbd "C-c <down>")    'windmove-down)
(global-set-key (kbd "C-c <left>")    'windmove-left)
(global-set-key (kbd "C-c <right>")   'windmove-right)

(global-set-key (kbd "C-c C-<up>")    'enlarge-window)
(global-set-key (kbd "C-c C-<down>")  'shrink-window)
(global-set-key (kbd "C-c C-<left>")  'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-<right>") 'shrink-window-horizontally)

;; EXWM CONFIG  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(load-file (expand-file-name "exwm-config.el" user-emacs-directory))

;; END-OF-INIT RELATED CONFIG  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; set GC back to normal. Value increased in early-init
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))

(unless (server-running-p)
  (server-start))

(require 'equake)
(equake-mode 1)
(advice-add #'save-buffers-kill-terminal :before-while #'equake-kill-emacs-advice)
(setq equake-default-shell 'term)


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; init.el ends here
