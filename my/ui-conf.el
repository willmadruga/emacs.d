;;; ui-conf.el --- UI config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'modus-themes)
(setq modus-themes-bold-constructs t
      modus-themes-mode-line '3d)
(modus-themes-load-themes)
(modus-themes-load-vivendi)

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)


(set-face-attribute 'default        nil :font "DejaVu Sans Mono" :height 100)
(set-face-attribute 'fixed-pitch    nil :font "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; ui-conf.el ends here
