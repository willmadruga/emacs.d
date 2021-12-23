;;; ui-conf.el --- UI config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(setup (:package spacemacs-theme)
  (set-face-attribute 'default        nil :font "Source Code Pro" :height 120)
  (set-face-attribute 'fixed-pitch    nil :font "Source Code Pro")
  (set-face-attribute 'variable-pitch nil :family "Source Code Pro")
  (load-theme 'spacemacs-dark t))

;; (defun wmad/spaceline-face-function (face, active)
;;   "My spaceline font setup"
;;   (cond
;;    (eq face 'mode-line) "Source Code Pro")
;;   )

(setup (:package spaceline)
  ;; turn off a couple of things from the default theme.
  (setq spaceline-buffer-position-p nil)
  (setq spaceline-hud-p nil)
  (setq spaceline-minor-modes-p nil)
  (setq spaceline-evil-state-p nil)
  (setq spaceline-auto-compile-p nil)
  (setq spaceline-buffer-size-p nil)

  ;; customize fonts.
  ;; (setq spaceline-face-func #'wmad/spaceline-face-function)
  (setq spaceline-face-func nil)
  (spaceline-spacemacs-theme))

(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; ui-conf.el ends here
