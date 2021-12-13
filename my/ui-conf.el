;;; ui-conf.el --- UI config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(setup (:package doom-themes)
  (load-theme 'doom-one t))

(setup (:package doom-modeline)
  (doom-modeline-mode 1))

(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)

(set-face-attribute 'default        nil :font "DejaVu Sans Mono" :height 100)
(set-face-attribute 'fixed-pitch    nil :font "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans")

;; Have Rougier's nano-emacs loaded from the command line.
(add-to-list 'load-path "/home/wmadruga/src/github/nano-emacs")
(add-to-list 'command-switch-alist '("-nano"   . (lambda (args))))
(cond
 ((member "-nano" command-line-args)
  (require 'nano-theme-dark)
  (require 'nano-layout)
  (require 'nano-faces)
  (nano-faces)
  (require 'nano-theme)
  (nano-theme)
  (require 'nano-modeline)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ))

;; https://www.emacswiki.org/emacs/TransparentEmacs
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; ;; Set transparency of emacs
;;  (defun set-transparency (value)
;;    "Sets the transparency of the frame window. 0=transparent/100=opaque"
;;    (interactive "nTransparency Value 0 - 100 opaque:")
;;    (set-frame-parameter (selected-frame) 'alpha value))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; ui-conf.el ends here
