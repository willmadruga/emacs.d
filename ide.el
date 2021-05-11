;;; ide.el --- IDE related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Attempt to have an IDE like environment.

;;; Code:

(wmad/package-install 'undo-fu)
(wmad/package-install 'browse-kill-ring)
(wmad/package-install 'move-text)

(wmad/package-install 'indent-guide)
(require 'indent-guide nil 'noerror)
(indent-guide-global-mode)


(wmad/package-install 'swiper)
(require 'swiper)
(setq swiper-action-recenter t)


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
;; (add-hook 'text-mode-hook 'ws-butler-mode)
;; (add-hook 'prog-mode-hook 'ws-butler-mode)


(wmad/package-install 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-text-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


(wmad/package-install 'dumb-jump)
(require 'dumb-jump)
(setq dumb-jump-default-project user-emacs-directory
	    dumb-jump-prefer-searcher 'rg
	    dumb-jump-aggressive nil)
(wmad/if-package 'ivy
                 (setq dumb-jump-selector 'ivy))


(wmad/package-install 'dired-single)
(require 'dired-single)
(require 'dired)
(define-key dired-mode-map [remap dired-find-file]
  'dired-single-buffer)
(define-key dired-mode-map [remap dired-mouse-find-file-other-window]
  'dired-single-buffer-mouse)
(define-key dired-mode-map [remap dired-up-directory]
  'dired-single-up-directory)


;; ;; (with-package 'company
;; ;;   (setq company-minimum-prefix-length 2
;; ;; 	      company-tooltip-limit 14
;; ;; 	      company-tooltip-align-annotations t
;; ;; 	      company-require-match 'never
;; ;; 	      company-global-modes '(not erc-mode message-mode help-mode gud-mode)
;; ;; 	      company-frontends '(company-pseudo-tooltip-frontend
;; ;; 		                        company-echo-metadata-frontend)
;; ;; 	      company-backends '(company-capf)
;; ;; 	      company-auto-complete nil
;; ;; 	      company-auto-complete-chars nil
;; ;; 	      company-dabbrev-other-buffers nil
;; ;; 	      company-dabbrev-ignore-case nil
;; ;; 	      company-dabbrev-downcase nil)
;; ;;   (add-hook 'after-init-hook 'global-company-mode))

;;; ide.el ends here
