;; wmad-core.el --- Core configuration : Entry-point -*- lexical-binding: t; -*-

;;; Commentary:

;; GC Optimizations learned from Doom Emacs: https://github.com/hlissner/doom-emacs/issues/310
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; oh boy, and it's fast! :)

;;; Code:

(require 'wmad-pkgsys)
(require 'wmad-emacs-config)
(require 'wmad-themes-config)
(require 'wmad-keys-config)
(require 'wmad-dired-config)

(require 'wmad-fns)
(require 'wmad-org)

(require 'wmad-base-pkgs)
(require 'wmad-devel-pkgs)

(require 'wmad-devel-lsp)
(require 'wmad-devel-js)
(require 'wmad-devel-clj)
(require 'wmad-devel-el)
(require 'wmad-devel-lisp)

(defun wmad/init ()
  "Emacs configuration initialization."
  (wmad/load-private-scripts)
  (wmad-pkgsys-init)
  
  (wmad/emacs-config)
  (wmad/keys-config)
  (wmad/theme-config)
  (wmad/org-config)
  (wmad/dired-config)

  (wmad/base-packages-init)
  (wmad/devel-packages-init)
  (wmad/devel-lsp-init)
  (wmad/devel-js-init)
  (wmad/devel-clj-init)
  (wmad/devel-elisp-init)
  (wmad/devel-lisp-init)
  )



(provide 'wmad-core)
;;; wmad-core.el ends here
