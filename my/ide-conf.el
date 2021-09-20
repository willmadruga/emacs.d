;;; ide-conf.el --- IDE-like config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(use-package emacs
  :ensure t
  :config
  (add-hook 'before-save-hook (lambda () (whitespace-cleanup))))

(use-package  projectile
  :ensure t
  :diminish "[Proj]"
  :config
  (autoload 'projectile-project-root "projectile")
  (projectile-mode +1))

(use-package indent-guide
  :ensure t
  :diminish
  :config
  (indent-guide-global-mode))

(use-package magit
  :ensure t
  :config
  (defalias 'git 'magit))

(use-package ibuffer-vc
  :hook (ibuffer-hook . (lambda ()
                          (require 'ibuf-ext)
                          (ibuffer-vc-set-filter-groups-by-vc-root)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic)))))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-text-hook . rainbow-delimiters-mode)
         (prog-mode-hook . rainbow-delimiters-mode)))

;; FIXME: Can I find a way to make my javascript projects work with
;; x-ref so I don't need to use dumb-jump again?
(use-package dumb-jump
  :ensure t

  ;; FIXME: keybindings should use new approach
  :bind (("C-c g g" . dumb-jump-go)
         ("C-c g b" . dumb-jump-back))

  :config
  (setq dumb-jump-default-project user-emacs-directory)
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-aggressive nil))

(use-package dired-single
  :ensure t
  :bind (([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
         ([remap dired-up-directory]                 . dired-single-up-directory)
         ([remap dired-find-file]                    . dired-single-buffer)))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode 1))

(use-package consult
  :ensure t
  :after (projectile)
  :bind (("C-c c a" . consult-apropos)
         ("C-c c b" . consult-buffer)
         ("C-c c c" . consult-flycheck)
         ("C-c c f" . consult-find)
         ("C-c c g" . consult-grep)
         ("C-c c l" . consult-line)
         ("C-c c m" . consult-imenu)
         ("C-c c r" . consult-ripgrep)
         ("C-c c R" . consult-recent-file)
         ("C-c c s" . consult-isearch))
  :config
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
   :preview-key (kbd "M-.")))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package orderless
  :ensure t
  :defer t
  :config
  (setq completion-styles '(orderless)))

(use-package corfu
  :ensure t
  :defer t
  :config
  (corfu-global-mode))

(use-package which-key
  :ensure t
  :diminish
  :defer t
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode 1))

(use-package diminish
  :ensure t
  :defer t
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'flymake-mode))

(use-package popper
  :ensure t
  :defer t
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\.*Async Shell Command.*"
          ))
  (setq popper-reference-modes
        '(help-mode
          compilation-mode))
  (popper-mode +1))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; ide-conf.el ends here
