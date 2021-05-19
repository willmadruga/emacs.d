;;; common.el --- Common Emacs configuration ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'recentf)
(setq recentf-max-saved-items 50)

(require 'shell)
(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "/usr/bin/zsh")


;; Emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq inhibit-startup-screen nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq undo-limit 80000000)
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "firefox")
(setq-default password-cache-expiry nil)
(setq-default gnutls-verify-error t)
(setq-default gnutls-min-prime-bits 2048)
(setq-default save-interprogram-paste-before-kill t)
(setq-default apropos-do-all t)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default vc-follow-symlinks t)
(setq-default ring-bell-function 'ignore)
;; (setq-default browse-url-browser-function 'eww-browse-url)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(delete-selection-mode 1)
(savehist-mode         1)


;; TAB
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


;; Buffer
(setq initial-major-mode 'org-mode)
(setq load-prefer-newer t)
(setq-default require-final-newline t)
(put 'narrow-to-region 'disabled nil)
(show-paren-mode       1)


;; Interface
(setq use-dialog-box t)
(setq-default fill-column 140)
(setq-default frame-resize-pixelwise t)
(setq-default mouse-yank-at-point t)
(global-eldoc-mode     1)
(electric-pair-mode    1)
(winner-mode           1)
(add-hook 'prog-mode-hook 'goto-address-mode)
(global-display-line-numbers-mode t)
(line-number-mode nil)
(column-number-mode -1)

(when (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'fringe-mode) (fringe-mode -1))

(dolist (mode '(org-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Environment
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


;; ALIASES
(defalias 'yes-or-no-p 'y-or-n-p)


;; Terminal Emacs only (emacs -nw)
(or (window-system)
    (progn
      (wmad/package-install 'clipetty)
      (require 'clipetty)
	    (global-clipetty-mode +1)
      (xterm-mouse-mode 1)
      (global-set-key (kbd "C-x ;") 'comment-line) ; "C-x C-;" is interpreted this way in some terminals
      ))


;; Scratch Buffer
(wmad/package-install 'persistent-scratch)
(require 'persistent-scratch)
(persistent-scratch-setup-default)
(setq persistent-scratch-backup-directory (concat user-emacs-directory "var/scratch"))
(add-hook 'kill-buffer-query-functions 'immortal-scratch)
(setq initial-scratch-message "#+title: Scratch Buffer\n\n"
      initial-major-mode 'org-mode)

;;; common.el ends here
