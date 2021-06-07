;;; ide.el --- IDE related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Attempt to have an IDE like environment.

;;; Code:

(wmad/package-install 'undo-fu)
(wmad/package-install 'move-text)
(wmad/package-install 'yasnippet-snippets)


(wmad/package-install 'projectile)
(require 'projectile)
(projectile-mode +1)


(wmad/package-install 'indent-guide)
(require 'indent-guide nil 'noerror)
(indent-guide-global-mode)


(wmad/package-install 'origami)
(require 'origami)
(global-origami-mode)


(wmad/package-install 'magit)
(require 'magit)
(defalias 'git 'magit)


(wmad/package-install 'hl-todo)
(require 'hl-todo nil 'noerror)
;; TODO: maybe change colors
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))
(global-hl-todo-mode)


(wmad/package-install 'ibuffer-vc)
(require 'ibuffer-vc)
(add-hook 'ibuffer-hook
	        (lambda ()
		        (ibuffer-vc-set-filter-groups-by-vc-root)
		        (unless (eq ibuffer-sorting-mode 'alphabetic)
		          (ibuffer-do-sort-by-alphabetic))))


(wmad/package-install 'ws-butler)
(require 'ws-butler)
(ws-butler-global-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)
;; (add-hook 'text-mode-hook 'ws-butler-mode)


(wmad/package-install 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-text-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


(wmad/package-install 'dumb-jump)
(require 'dumb-jump)
(setq dumb-jump-default-project user-emacs-directory
	    dumb-jump-prefer-searcher 'rg
	    dumb-jump-aggressive nil)


(wmad/package-install 'dired-single)
(require 'dired-single)
(require 'dired)
(define-key dired-mode-map [remap dired-find-file]
  'dired-single-buffer)
(define-key dired-mode-map [remap dired-mouse-find-file-other-window]
  'dired-single-buffer-mouse)
(define-key dired-mode-map [remap dired-up-directory]
  'dired-single-up-directory)


(wmad/package-install 'auto-dim-other-buffers)
(require 'auto-dim-other-buffers)
(setq auto-dim-other-buffers-dim-on-focus-out t)
(setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
(auto-dim-other-buffers-mode t)


(wmad/package-install 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)


(wmad/package-install 'consult)
(wmad/package-install 'consult-flycheck)
(require 'consult)
(require 'projectile)
(autoload 'projectile-project-root "projectile")
(setq consult-project-root-function #'projectile-project-root)

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)


(consult-customize
 consult-ripgrep
 consult-git-grep
 consult-grep
 consult-recent-file
 :preview-key (kbd "M-."))


(wmad/package-install 'consult-lsp)
(define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)


(wmad/package-install 'vertico)
(require 'vertico)
(vertico-mode)


(wmad/package-install 'marginalia)
(require 'marginalia)
(marginalia-mode)


(wmad/package-install 'orderless)
(require 'orderless)
(setq completion-styles '(orderless))


(wmad/if-package 'mini-popup
  (and (window-system)
       (mini-popup-mode)
       (add-hook 'consult--completion-refresh-hook #'mini-popup--setup 99) ;; Ensure that the popup is updated after refresh (Consult-specific)
       ;; Configure a height function (Example for Vertico)
       ;; (defun mini-popup-height-resize ()
       ;;   (* (1+ (min vertico--total vertico-count)) (default-line-height)))
       ;; (defun mini-popup-height-fixed ()
       ;;   (* (1+ (if vertico--input vertico-count 0)) (default-line-height)))
       ;; (setq mini-popup--height-function #'mini-popup-height-fixed)
       ;; ;; Disable the minibuffer resizing of Vertico (HACK)
       ;; (advice-add #'vertico--resize-window :around
       ;;             (lambda (&rest args)
       ;;               (unless mini-popup-mode
       ;;                 (apply args))))
       ))

(wmad/package-install 'corfu)
(require 'corfu)
(corfu-global-mode)

;;; ide.el ends here
