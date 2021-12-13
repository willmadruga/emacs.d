;;; keyb-conf.el --- Key Bindings config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'popper)
(require 'org-roam)
(require 'org-roam-dailies)
(require 'eglot)
(require 'js2-mode)
(require 'elfeed)
(require 'sdfcli "../../my/sdfcli.el")

;; Trying it out... not too bad, just don't know if I will stick with it yet...
;; https://github.com/meow-edit/meow
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-leader-define-key
   '("a" . avy-goto-char-timer)
   '("b" . consult-buffer)
   '("B" . ibuffer)
   '("d" . crux-duplicate-current-line-or-region)
   '("D" . netsuite/deploy)
   '("e" . embark-act)
   '("E" . elfeed)
   '("f" . eglot-format)
   '("g" . consult-ripgrep)
   '("i" . imenu)
   '("o" . org-roam-dailies-map)
   '("O" . org-roam-node-find)
   '("p" . popper-toggle-latest)
   '("r" . eglot-rename)
   '("t" . ansi-term)
   '("u" . netsuite/upload-buffer)
   '("w" . wdired-change-to-wdired-mode)
   '("z" . js2-mode-toggle-element)
   '("." . dumb-jump-go)
   '("," . dumb-jump-back)
   '("/" . consult-line)
   )
  )

(setup (:package meow)
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1))

;;; keyb-conf.el ends here
