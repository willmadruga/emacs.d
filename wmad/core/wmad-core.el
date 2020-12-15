;; wmad-core.el --- Core configuration : Entry-point
;; -*- lexical-binding: t; -*-

;;; Commentary:

;; GC Optimizations learned from Doom Emacs: https://github.com/hlissner/doom-emacs/issues/310
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

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

(defun wmad/pre-init ()
  "Optimizations before init."

  ;; Increase GC threshold
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

  ;; temporarily unset file-name-handler-alist
  (defvar wmad--file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  )

(defun wmad/init ()
  "Emacs configuration initialization."
  (wmad/load-private-scripts)

  (wmad/emacs-config)
  (wmad-pkgsys-init)
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

(defun wmad/post-init ()
  "Post-Initialization tasks"

  ;; after startup, it is important you reset the GC back to some reasonable default.
  ;; A large gc-cons-threshold will cause freezing and stuttering during long-term 
  ;; interactive use. I find these are nice defaults:
  (add-hook 'emacs-startup-hook
            (lambda ()
              ;; decrease GC threshold
              (setq gc-cons-threshold 16777216 ; 16mb
                    gc-cons-percentage 0.1)
              ;; restore file-name-handler-alist
              (setq file-name-handler-alist wmad--file-name-handler-alist)))
  )

(provide 'wmad-core)
;;; wmad-core.el ends here
