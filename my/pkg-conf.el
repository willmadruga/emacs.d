;;; pkg-conf.el --- Packages config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(setq package-archives
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("GNU ELPA" . 10)
        ("MELPA"    . 5)))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(dolist (pname
         '(
           ;; Theme
           modus-themes mixed-pitch
           all-the-icons all-the-icons-dired all-the-icons-ibuffer all-the-icons-completion

           ;; Environment
           gcmh snitch
           consult consult-flycheck vertico marginalia orderless
           ibuffer-vc dired-single which-key crux diminish popper
           move-text dumb-jump corfu origami indent-guide rainbow-delimiters
           major-mode-hydra

           ;; Org
           org-roam org-superstar

           ;; IDE
           projectile magit js2-mode eglot flycheck yasnippet yasnippet-snippets restclient jira-markup-mode
           helpful devdocs-browser

           ;; EXWM
           exwm exwm-edit desktop-environment
           ))
  (unless (package-installed-p pname)
    (progn
      (package-refresh-contents)
      (package-install pname))))

;;; pkg-conf.el ends here
