;;; misc.el --- Miscelanea related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(wmad/package-install 'page-break-lines)


(wmad/package-install 'dashboard)
(require 'project)
(require 'dashboard)
(require 'page-break-lines)
(setq dashboard-set-init-info t)
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(setq dashboard-banner-logo-title nil)
(setq dashboard-set-navigator nil)

(setq dashboard-startup-banner 'logo)
(setq dashboard-page-separator "\n\n")
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-footer-messages '("\"Imagine all the people, living life in peace.\""))
(setq dashboard-projects-backend 'project-el)
(setq dashboard-items '((recents  . 10)
			                  ;; (projects . 10)
			                  (agenda . 10)
			                  (bookmarks . 10)
			                  (registers . 10)
                        ))

(dashboard-setup-startup-hook)


(wmad/package-install 'modus-themes)
(require 'modus-themes)
(setq modus-themes-bold-constructs t
	    modus-themes-mode-line '3d)
(modus-themes-load-themes)
(modus-themes-load-vivendi)


(wmad/package-install 'doom-modeline)
(require 'doom-modeline)
(doom-modeline-mode 1)
(add-hook 'after-init-hook #'doom-modeline-mode)


(setq display-buffer-alist

      ;;TODO: always display shell on a dedicated, reusable window, add a toggling function, bind to a key

      ;; display the window for the async shell command output at the bottom, small height.
      '(("\\.*Async Shell Command.*"
         (display-buffer-at-bottom)
         (window-height . 0.1)
         )))


;;; misc.el ends here
