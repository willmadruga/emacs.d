;; wmad-devel-pkgs.el ---  --- Development Configuration Layer: Base packages

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
  (use-package ag :ensure t)

  ;; ripgrep search ;;;;;;;;;;;;;
  (use-package ripgrep :ensure t)

  ;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package magit
    :ensure t
    :custom
    (magit-display-buffer-function
     #'magit-display-buffer-same-window-except-diff-v1))

  ;; rainbow delimiters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package rainbow-delimiters
    :ensure t
    :diminish
    :hook (prog-mode-hook . rainbow-delimiters-mode))

  ;; YASnippet ;;;;;;;;;;;;;;;;;
  (use-package yasnippet
    :ensure t
    :config (yas-global-mode 1))

  ;; YASnippet snippets ;;;;;;;;;;;;;;;;;;;;
  (use-package yasnippet-snippets :ensure t)

  ;; flycheck ;;;;;;;;;;;;;;;
  (use-package flycheck
    :ensure t
    :init
    (global-flycheck-mode t))

  ;; smart parens ;;;;;;;;;;;;;;;;;;;
  (use-package smartparens :ensure t)

  ;; Origami - folding mechanism
  (use-package origami
    :ensure t
    :config
    (global-origami-mode))

  ;; indent guide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package indent-guide
    :ensure t
    :hook (prog-mode-hook . indent-guide-mode))

  ;; restclient ;;;;;;;;;;;;;;;;;;;;
  (use-package restclient :ensure t)

  ;; Dumb Jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package dumb-jump
    :ensure t
    :config
    (add-hook 'xref-backend-functions
              #'dumb-jump-xref-activate))

  )

(provide 'wmad-devel-pkgs)
;;; wmad-devel-pkgs.el ends here
