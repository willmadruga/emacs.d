;;; init.el --- Emacs init ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'package)
(require 'server)

(setq package-archives
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("GNU ELPA" . 10)
        ("MELPA"    . 5)))

;; Package init and install setup.el
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(if (package-installed-p 'setup) nil
  (if (memq 'setup package-archive-contents) nil
    (package-refresh-contents))
  (package-install 'setup))

;; load each configuration file.
(dolist (fname
         '("my/perf-conf.el"
           "my/conf.el"
           "my/ide-conf.el"
           "my/js-conf.el"
           "my/sdfcli.el"
           "my/ui-conf.el"
           "my/elfeed-config.el"
           "my/org-conf.el"
           "my/keyb-conf.el"
           "password-store.el"
           ))
  (load-file (expand-file-name fname user-emacs-directory)))

;; set GC back to normal. Value increased in early-init
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; turn server mode ON
(unless (server-running-p) (server-start))

;;; init.el ends here
