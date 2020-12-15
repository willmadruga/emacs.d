;; wmad-themes-config.el --- Themes Configuration Layer
;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/theme-config ()
  (use-package modus-vivendi-theme
    :defer t
    :ensure t)

  (use-package modus-operandi-theme
    :defer t
    :ensure t)

  (use-package all-the-icons
    :defer t    
    :ensure t)

  (use-package doom-modeline
    :defer t    
    :ensure t
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 15)))

  (use-package dashboard
    :ensure t
    :config
    (setq dashboard-items
          '((recents . 5)
            (projects . 5)
            (bookmarks . 5)
            (agenda . 20)))
    (setq dashboard-set-init-info t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (dashboard-modify-heading-icons '((recents . "file-text")
                                      (bookmarks . "book")))
    (dashboard-setup-startup-hook))

  (use-package emacs
    :config
    (load-theme 'modus-vivendi t)

    (defvar wmad/default-font-size 100)

    (set-face-attribute 'default nil :font "Roboto Mono Light" :height wmad/default-font-size)

    ;; Set the fixed pitch face
    ;;(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height wmad/default-font-size)
    ;;(set-face-attribute 'fixed-pitch nil :font "Roboto Mono Light" :height wmad/default-font-size)
    (set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height wmad/default-font-size)
    ;;(set-face-attribute 'fixed-pitch nil :font "Noto Sans Italic" :height wmad/default-font-size)

    ;; Set the variable pitch face
    ;; (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'regular)
    (set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height wmad/default-font-size :weight 'regular))

  )

(provide 'wmad-themes-config)
;;; wwmad-themes-config.el ends here
