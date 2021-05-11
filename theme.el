;;; misc.el --- Miscelanea related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(wmad/package-install 'dashboard)
(require 'dashboard)
(setq dashboard-center-content t)
(setq dashboard-startup-banner 'logo)
(setq dashboard-banner-logo-title nil)
(setq dashboard-page-separator "\n\n")
(setq dashboard-set-init-info t)
(setq dashboard-footer-messages '("\"Imagine all the people, living life in peace.\""))
(setq dashboard-items '((recents  . 10)
			                  (agenda . 20)))
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


(wmad/package-install 'dashboard)
(require 'dashboard)

(setq dashboard-center-content t)

(setq dashboard-startup-banner 'logo)
(setq dashboard-banner-logo-title nil)
(setq dashboard-page-separator "\n\n")
(setq dashboard-set-init-info t)
(setq dashboard-footer-messages '("\"Imagine all the people, living life in peace.\""))
(setq dashboard-items '((recents  . 10)
			                  (agenda . 20)))
(dashboard-setup-startup-hook)


(add-hook 'kill-buffer-query-functions 'immortal-scratch)
(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)
(run-with-idle-timer 300 t 'save-persistent-scratch)

(setq display-buffer-alist

      ;;TODO: always display shell on a dedicated, reusable window, add a toggling function, bind to a key

      ;; display the window for the async shell command output at the bottom, small height.
      '(("\\.*Async Shell Command.*"
         (display-buffer-at-bottom)
         (window-height . 0.1)
         )))


;;; misc.el ends here
