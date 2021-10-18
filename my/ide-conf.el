;;; ide-conf.el --- IDE-like config ;; -*- lexical-binding: t; -*-
;; This file is NOT part of GNU Emacs.
;;; Commentary:
;;; Code:

(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

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
  (:hook (lambda ()
           (ibuffer-vc-set-filter-groups-by-vc-root)
           (unless (eq ibuffer-sorting-mode 'alphabetic)
             (ibuffer-do-sort-by-alphabetic)))))

(setup (:package rainbow-delimiters)
  (:hook-into prog-text-hook)
  (:hook-into prog-mode-hook))

(setup (:package dumb-jump)
  (setq dumb-jump-default-project user-emacs-directory)
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-aggressive nil))

(setup (:package dired-single)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]                 'dired-single-up-directory)
  (define-key dired-mode-map [remap dired-find-file]                    'dired-single-buffer))

(setup (:package yasnippet)
  (yas-global-mode 1))

(setup (:package consult)
  (:require projectile)
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

;; TODO: learned recently this can be replaced by embark... and present results in a much better way
(setup (:package which-key)
  (:hide-mode)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode 1))

(setup (:package popper)
  ;; see docs https://github.com/karthink/popper
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          ))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; ide-conf.el ends here
