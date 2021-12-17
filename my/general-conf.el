;;; general-conf.el --- General Emacs config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'recentf)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-default-init                                t)
(setq load-prefer-newer                                   t)
(setq make-backup-files                                 nil)
(setq create-lockfiles                                  nil)
(setq browse-url-browser-function       'browse-url-generic)
(setq browse-url-generic-program                  "firefox")
(setq read-process-output-max                 (* 1024 1024))
(setq warning-suppress-types                      '((comp)))
(setq recentf-max-saved-items                            50)
(setq undo-limit                                   80000000)
(setq tab-always-indent                           'complete)
(setq winner-dont-bind-my-keys                            t)
(setq initial-major-mode                   'emacs-lisp-mode)
(setq shell-file-name                       "/usr/bin/bash")

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

(dolist (mode '(org-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-display-line-numbers-mode  t)
(put 'narrow-to-region 'disabled nil)
(add-to-list 'recentf-exclude (concat user-emacs-directory "elpa"))

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system       'utf-8)

(delete-selection-mode 1)
(global-eldoc-mode     1)
(electric-pair-mode    1)
(winner-mode           1)
(column-number-mode    1)
(show-paren-mode       1)
(savehist-mode         1)
(recentf-mode          1)
(tab-bar-mode          0)
(line-number-mode    nil)

;; TODO: use setup if-host to build a $PATH for each system.
(dolist
    (item '("~/bin"
            "~/.local/bin"
            "~/.nix-profile/bin"
            "~/.deno/bin"
            "~/.npm-packages/bin"
            ))
  (add-to-list 'exec-path item))
(setenv "PATH" (string-trim-right (string-join exec-path ":") ":$"))
(setenv "SHELL" shell-file-name)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; general-conf.el ends here
