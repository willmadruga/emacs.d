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
             ;; set of initial config and utility functions
             "utils.el"
             "custom.el"
             "clean.el"
             "perf.el"
             "common.el"
             "mini-popup/mini-popup.el"

             ;; testing it out...
             ;; https://github.com/emacsmirror/emacswiki.org/blob/master/sr-speedbar.el
             ;; commit f7f3b398decd4dc66b85289ac3926a03f8750afc
             ;; Jan 7, 2020
             "sr-speedbar.el"


             ;; Work IDE
             "ide.el"
             "sdfcli.el"
             "js.el"


             ;; Extras
             "org.el"
             "misc.el"
             "theme.el"
             "password-store.el"
             ;; "svg-lib/svg-lib.el"


             ;; Bindings
             "bindings.el"
             "hydra.el"
             ;; TODO: Embark
             ))
  (let ((f (expand-file-name c user-emacs-directory)))
    (when (file-exists-p f) (load-file f))))

;;; init.el ends here
