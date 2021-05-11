;;; init.el --- Emacs init ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA" . 5)))
(package-initialize)

;; Load each configuration file if existent
(dolist (c '(
             ;; set of initial configuration and utility functions
             "utils.el"
             "custom.el"
             "clean.el"
             "perf.el"
             "common.el"

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
             ))
  (let ((f (expand-file-name c user-emacs-directory)))
    (when (file-exists-p f) (load-file f))))

;;; init.el ends here
