;;; early-init.el --- Emacs early init ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; To keep Emacs from automatically making packages available at
;; startup, change the variable ‘package-enable-at-startup’ to ‘nil’.  You
;; must do this in the early init file, as the variable is read before
;; loading the regular init file.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
