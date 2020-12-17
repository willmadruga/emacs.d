;; wmad-emacs-config.el --- Emacs Configuration Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/emacs-config ()
  "Emacs configuration."
  (use-package emacs
    :init
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (display-time-mode 1)
    (column-number-mode 1)
    (show-paren-mode 1)
    (set-fringe-mode 10)

    :config
    (setq use-file-dialog nil)
    (setq use-dialog-box t)
    (setq visible-bell t)
    (setq initial-scratch-message "")
    (setq undo-limit 80000000)
    (setq make-backup-files nil)
    (setq create-lockfiles nil)

    (fset 'yes-or-no-p 'y-or-n-p)
    (delete-selection-mode 1)
    (toggle-frame-fullscreen)
    (column-number-mode)

    (unless (equal "Battery status not available" (battery))
      (display-battery-mode 1))

    ;; line numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (global-display-line-numbers-mode t)
    (dolist (mode '(org-mode-hook
                    shell-mode-hook
                    eshell-mode-hook
                    term-mode-hook))
      (add-hook mode (lambda () (display-line-numbers-mode 0))))

    ;; Enable narrowing
    (put 'narrow-to-region 'disabled nil)
    
    )
  
  (use-package recentf
    :config
    (setq recentf-max-saved-items 50)
    (recentf-mode t))

  (use-package window
  :init
  (setq display-buffer-alist
        '(
          ("^\\(\\*Bufler.*\\|\\*Help.*\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.40)
           (side . right)
           (slot . 1))

          ("^\\(\\*ag.*\\|\\*cider-repl.*\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.30)
           (side . bottom)
           (slot . 1))


          ("^\\(\\*e?shell\\|vterm\\|*HTTP.*\\|*Async.*\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.15)
           (side . bottom)
           (slot . 0))))

  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  :hook ((help-mode-hook . visual-line-mode)
         (custom-mode-hook . visual-line-mode)))


  ) ;; wmad/emacs-config

(provide 'wmad-emacs-config)
;;; wmad-emacs-config.el ends here
