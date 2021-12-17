;;; ui-conf.el --- UI config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

;; (defun wmad/use-doom-theme ()
;;   "Use the doom theme with doom modeline."
;;   (setup (:package doom-themes)
;;     (load-theme 'doom-one t))
;;   (setup (:package doom-modeline)
;;     (doom-modeline-mode 1))

;;   (set-face-attribute 'default        nil :font "DejaVu Sans Mono" :height 100)
;;   (set-face-attribute 'fixed-pitch    nil :font "DejaVu Sans Mono")
;;   (set-face-attribute 'variable-pitch nil :family "DejaVu Sans"))

;; (defun wmad/use-nano-theme ()
;;   "Use NANO theme."

;;   (add-to-list 'load-path "/home/wmadruga/src/github/nano-emacs")
;;   (require 'nano-theme-dark)
;;   (require 'nano-layout)
;;   (require 'nano-faces)
;;   (nano-faces)
;;   (require 'nano-theme)
;;   (nano-theme)
;;   (menu-bar-mode -1)
;;   (tool-bar-mode -1)
;;   (scroll-bar-mode -1)
;;   (require 'nano-modeline))

(defun wmad/use-spacemacs-theme ()
  "Use Spacemacs theme."
  (setup (:package spacemacs-theme)
    (set-face-attribute 'default        nil :font "Roboto Mono" :height 120)
    (set-face-attribute 'fixed-pitch    nil :font "Roboto Mono")
    (set-face-attribute 'variable-pitch nil :family "Roboto")
    (load-theme 'spacemacs-dark t nil)))

(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)
(wmad/use-spacemacs-theme)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; ui-conf.el ends here
