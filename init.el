;;; init.el --- Emacs init ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA" . 10)
        ("MELPA"    . 5)))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Load each config file, if existent
(dolist (c '(
             ;; initial config and utility functions
             "utils.el"
             "custom.el"
             "perf.el"
             "clean.el"
             "common.el"
             ;; "mini-popup/mini-popup.el"

             ;; Work IDE
             "ide.el"
             "sdfcli.el"
             "js.el"


             ;; Extras
             "org.el"
             "misc.el"
             "theme.el"
             "password-store.el"

             ;; Bindings
             "bindings.el"
             "hydra.el"
             ;; TODO: Embark
             ))
  (let ((f (expand-file-name c user-emacs-directory)))
    (when (file-exists-p f) (load-file f))))

;;; init.el ends here
