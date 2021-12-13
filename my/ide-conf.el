;;; ide-conf.el --- IDE-like config ;; -*- lexical-binding: t; -*-
;; This file is NOT part of GNU Emacs.
;;; Commentary:
;;; Code:

(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

(setup (:package embark))
;; explore:
;; https://github.com/oantolin/embark/wiki/Additional-Actions
;; https://github.com/oantolin/embark/wiki/Additional-Configuration
;; https://karthinks.com/software/fifteen-ways-to-use-embark/

(setup (:package crux))
(setup (:package avy))
(setup (:package restclient))
(setup (:package devdocs))
(setup (:package origami)
  (global-origami-mode))

(setup (:package projectile)
  (autoload 'projectile-project-root "projectile")
  (projectile-mode +1))

(setup (:package move-text)
  (move-text-default-bindings))

(setup (:package indent-guide)
  (indent-guide-global-mode))

(setup (:package magit)
  (defalias 'git 'magit))

(setup (:package ibuffer-vc)
  (:require ibuf-ext)
  (:with-hook ibuffer-hook
    (:hook (lambda ()
             (ibuffer-vc-set-filter-groups-by-vc-root)
             (unless (eq ibuffer-sorting-mode 'alphabetic)
               (ibuffer-do-sort-by-alphabetic))))))

(setup (:package rainbow-delimiters)
  (:hook-into prog-text-hook)
  (:hook-into prog-mode-hook))

(setup (:package dumb-jump)
  (setq dumb-jump-default-project user-emacs-directory)
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-aggressive nil)
  (dumb-jump-mode 1))

(setup (:package dired-single)
  (require 'dired)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]                 'dired-single-up-directory)
  (define-key dired-mode-map [remap dired-find-file]                    'dired-single-buffer))

(setup (:package yasnippet)
  (yas-global-mode 1))

(setup (:package consult)
  (:require projectile)
  (require 'consult)
  (setq consult-project-root-function #'projectile-project-root)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))

  (consult-customize consult-line :keymap my-consult-line-map)

  (consult-customize
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-recent-file
   :preview-key (kbd "M-.")))

(setup (:package vertico)
  (vertico-mode))

(setup (:package marginalia)
  (marginalia-mode))

(setup (:package orderless)
  (setq completion-styles '(orderless)))

(setup (:package corfu)
  (corfu-global-mode))

(setup (:package popper)
  ;; see docs https://github.com/karthink/popper
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*slime-repl sbcl\\*"
          help-mode
          compilation-mode
          ))
  (popper-mode +1)
  (popper-echo-mode +1))

(setup (:package pomm)
  (require 'pomm)
  (setq pomm-mode-line-mode t)
  (setq alert-default-style 'libnotify)
  (setq pomm-audio-enabled t)
  (setq pomm-audio-player-executable "mpv"))


;; lisp
(setup (:package slime)
    (setq inferior-lisp-program "/usr/local/bin/sbcl"))
(setup (:package slime-repl-ansi-color))
(setup (:package ac-slime)
   (require 'ac-slime)
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    (eval-after-load "auto-complete"
      '(add-to-list 'ac-modes 'slime-repl-mode)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; ide-conf.el ends here
