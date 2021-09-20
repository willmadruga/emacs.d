;;; ide-conf.el --- IDE-like config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

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

(require 'popper)
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "\\.*Async Shell Command.*"
        "\\.*EGLOT.* events.*"
        "\\.*XELB-DEBUG*"
        help-mode
        compilation-mode
        ))
(popper-mode +1)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; ide-conf.el ends here
