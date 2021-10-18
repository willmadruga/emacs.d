;;; ui-conf.el --- UI config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(setup (:package modus-themes)

  (setq modus-themes-bold-constructs t
        modus-themes-mode-line '3d)
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

(setup (:package doom-modeline)
  (doom-modeline-mode 1))

(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)

(set-face-attribute 'default        nil :font "DejaVu Sans Mono" :height 100)
(set-face-attribute 'fixed-pitch    nil :font "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans")

;; TESTING IT OUT
;; (setup svg-tag-mode
;;   (:package svg-tag-mode))
;; see https://github.com/rougier/svg-tag-mode

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; ui-conf.el ends here
