;; wmad-themes-config.el --- Themes Configuration Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/theme-config ()
  "Emacs \"theme\" configuration."

  (use-package modus-vivendi-theme
    :disabled t
    :ensure t
    :defer t
    :config
    (load-theme 'modus-vivendi t))

  (use-package modus-operandi-theme
    :disabled t
    :ensure t
    :defer t)

  (use-package all-the-icons
    :ensure t
    :defer t)

  (use-package doom-themes
    :ensure t
    :config

    (load-theme 'doom-one t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Corrects (and improves) org-mode's native 'fontification'.
    (doom-themes-org-config))

  (use-package doom-modeline
    :ensure t
    :defer t
    :init (doom-modeline-mode 1)
    :custom-face
    (mode-line ((t (:height 1.0))))
    (mode-line-inactive ((t (:height 0.90))))
    :custom
    (doom-modeline-height 15)
    (doom-modeline-bar-width 10)
    (doom-modeline-lsp t)
    (doom-modeline-minor-modes t))

  (use-package emacs
    :config

    (defvar wmad/default-font-size 100)

    (set-face-attribute 'default nil :font "Roboto Mono Light" :height wmad/default-font-size)

    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height wmad/default-font-size)

    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height wmad/default-font-size :weight 'regular))

  )

(provide 'wmad-themes-config)

;;; wmad-themes-config.el ends here
