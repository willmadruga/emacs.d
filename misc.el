;;; misc.el --- Miscelanea related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(wmad/package-install 'restart-emacs)
(wmad/package-install 'hnreader)
(wmad/package-install 'writefreely)
(wmad/package-install 'crux)
(wmad/package-install 'helpful)


(wmad/package-install 'olivetti)
(require 'olivetti)
(setq olivetti-body-width 165)


(wmad/package-install 'elpher)
(require 'elpher)
(setq gnutls-verify-error 'nil)


(wmad/package-install 'emacs-everywhere)
(require 'emacs-everywhere)
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


(wmad/package-install 'ivy)
(require 'ivy)
(ivy-mode 1)
(setq ivy-sort-max-size 7500)
(setq ivy-height 17)
(setq ivy-wrap t)
(setq ivy-fixed-height-minibuffer t)
(setq ivy-read-action-function #'ivy-hydra-read-action)
(setq ivy-read-action-format-function #'ivy-read-action-format-columns)
(setq ivy-use-virtual-buffers nil)
(setq ivy-virtual-abbreviate 'full)
(setq ivy-on-del-error-function #'ignore)
(setq ivy-use-selectable-prompt t)
(setq ivy-initial-inputs-alist nil)


(wmad/package-install 'counsel)
(require 'counsel nil 'noerror)
(setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")


(wmad/package-install 'prescient)
(require 'prescient)
(prescient-persist-mode 1)



(wmad/package-install 'ivy-prescient)
(require 'ivy-prescient)
(ivy-prescient-mode 1)


;; Configure ivy-posframe after other packages so I can map them.
(wmad/package-install 'ivy-posframe)
(require 'ivy-posframe)
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
(ivy-posframe-mode 1)


;;; misc.el ends here
