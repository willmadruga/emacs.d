;;; init.el --- Emacs init ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; TEMPORARY ;; TEMPORARY ;; TEMPORARY ;; TEMPORARY ;; TEMPORARY ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq user-emacs-directory "~/emacs.min/")                         ;;
;; (customize-set-value 'custom-theme-directory user-emacs-directory) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEMPORARY ;; TEMPORARY ;; TEMPORARY ;; TEMPORARY ;; TEMPORARY ;;;

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA" . 5)))
(package-initialize)

;; Load each configuration file if existent
(dolist (c '(
             ;; set of initial configuration and utility functions
             "utils.el"
             "custom.el"
             "clean.el"
             "perf.el"
             "common.el"

             ;; Work IDE
             "ide.el"
             "sdfcli.el"
             "js.el"

             ;; Extras
             "org.el"
             "misc.el"
             "theme.el"

             ;; Bindings
             "bindings.el"
             "hydra.el"
             ))
  (let ((f (expand-file-name c user-emacs-directory)))
    (when (file-exists-p f) (load-file f))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(olivetti-body-width 165)
 '(package-selected-packages
   '(hydra doom-modeline modus-themes dashboard ivy-posframe ivy-prescient prescient counsel emacs-everywhere elpher olivetti helpful crux writefreely hnreader restart-emacs org-brain flycheck eglot fill-column-indicator js2-refactor dired-single dumb-jump rainbow-delimiters ws-butler ibuffer-vc hl-todo magit origami swiper indent-guide move-text browse-kill-ring undo-fu no-littering)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
