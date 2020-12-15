;; wmad-devel-pkgs.el ---  --- Development Configuration Layer: Base packages
;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-packages-init ()

  ;; General settings
  (use-package emacs
    :config
    (setq-default indent-tabs-mode nil
                  fill-column 140
                  tab-width 2)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))

  ;; Projectile
  (use-package projectile
    :defer t
    :ensure t
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ido))
    :bind-keymap ("C-c p" . projectile-command-map)
    :init
    (when (or (file-directory-p "~/src") (file-directory-p "~/git"))
      (setq projectile-project-search-path '("~/src" "~/git")))
    (setq projectile-switch-project-action #'projectile-dired))

  ;; ag search ;;;;;;;;;;;;;
  (use-package ag
    :defer t
    :ensure t)

  ;; ripgrep search ;;;;;;;;;;;;;
  (use-package ripgrep
    :defer t
    :ensure t)

  ;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package magit
    :defer t
    :ensure t
    :custom
    (magit-display-buffer-function
     #'magit-display-buffer-same-window-except-diff-v1))

  ;; rainbow delimiters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package rainbow-delimiters
    :defer t
    :ensure t
    :diminish
    :hook (prog-mode-hook . rainbow-delimiters-mode))

  ;; YASnippet ;;;;;;;;;;;;;;;;;
  (use-package yasnippet
    :defer t
    :ensure t
    :config (yas-global-mode 1))

  ;; YASnippet snippets ;;;;;;;;;;;;;;;;;;;;
  (use-package yasnippet-snippets
    :defer t
    :ensure t)

  ;; flycheck ;;;;;;;;;;;;;;;
  (use-package flycheck
    :defer t
    :ensure t
    :init
    (global-flycheck-mode t))

  ;; smart parens ;;;;;;;;;;;;;;;;;;;
  (use-package smartparens
    :defer t
    :ensure t)

  ;; Origami - folding mechanism
  (use-package origami
    :defer t
    :ensure t
    :config
    (global-origami-mode))

  ;; indent guide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package indent-guide
    :defer t
    :ensure t
    :hook (prog-mode-hook . indent-guide-mode))

  ;; restclient ;;;;;;;;;;;;;;;;;;;;
  (use-package restclient
    :defer t
    :ensure t)

  ;; Dumb Jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package dumb-jump
    :defer t
    :ensure t
    :config
    (add-hook 'xref-backend-functions
              #'dumb-jump-xref-activate))

  )

(provide 'wmad-devel-pkgs)
;;; wmad-devel-pkgs.el ends here
