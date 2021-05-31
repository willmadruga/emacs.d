;;; init.el --- Emacs init ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA" . 5)))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Load each config file, if existent
(dolist (c '(
             ;; set of initial config and utility functions
             "utils.el"
             "custom.el"
             "clean.el"
             "perf.el"
             "common.el"

             ;;	https://github.com/minad/mini-popup.git
             ;; commit 30d97565419a788b5a03a550c957664c088359cf
             ;; May 24, 2021
             "mini-popup.el"

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


             ;; Bindings
             "bindings.el"
             "hydra.el"
             ;; TODO: Embark
             ))
  (let ((f (expand-file-name c user-emacs-directory)))
    (when (file-exists-p f) (load-file f))))

;;; init.el ends here
