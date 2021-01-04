;; wmad-devel-pkgs.el ---  --- Development Configuration Layer: Base packages -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-packages-init ()
  "Development packages configuration."
  
  ;; General settings
  (use-package emacs
    :config
    (setq-default indent-tabs-mode nil
                  fill-column 140
                  tab-width 2)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))

  ;; Projectile
  (use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ido))
    :bind-keymap ("C-c p" . projectile-command-map)
    :init
    (when (or (file-directory-p "~/src") (file-directory-p "~/git"))
      (setq projectile-project-search-path '("~/src" "~/git")))
    (setq projectile-switch-project-action #'projectile-dired))

  ;; ag search ;;;;;;;;;;;;;
  (use-package ag)

  ;; ripgrep search ;;;;;;;;;;;;;
  (use-package ripgrep)
  
  ;; rainbow delimiters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package rainbow-delimiters
    :diminish
    :hook (prog-mode-hook . rainbow-delimiters-mode))

  ;; YASnippet ;;;;;;;;;;;;;;;;;
  (use-package yasnippet
    :config (yas-global-mode 1))
  
  ;; YASnippet snippets ;;;;;;;;;;;;;;;;;;;;
  (use-package yasnippet-snippets)
  ;; flycheck ;;;;;;;;;;;;;;;
  (use-package flycheck
    :hook (prog-mode-hook . global-flycheck-mode)
    :config
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (setq-default flycheck-temp-prefix ".flycheck")
    
    ;; disable jshint
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)
                          '(json-jsonlist)))
    ;; enable eslint
    (flycheck-add-mode 'javascript-eslint 'js2-mode))

  ;; smart parens ;;;;;;;;;;;;;;;;;;;
  (use-package smartparens    
    :hook (prog-mode-hook . smartparens-mode))

  ;; Origami - folding mechanism
  (use-package origami
    :hook (prog-mode-hook . global-origami-mode))

  ;; indent guide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package indent-guide
    :hook (prog-mode-hook . indent-guide-mode))

  ;; restclient ;;;;;;;;;;;;;;;;;;;;
  (use-package restclient)
  
  ;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package magit
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  ;; display TODO and similars in repo ;;
  (use-package magit-todos)

  (use-package dumb-jump
    ;; :hook (xref-backend-functions . dumb-jump-xref-activate)
    :config
    (setq dumb-jump-disable-obsolete-warnings t)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    )
  
  ;; parinfer, infer changes to parens ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (use-package parinfer
  ;;   :hook ((clojure-mode . parinfer-mode)
  ;;          (emacs-lisp-mode . parinfer-mode)
  ;;          (common-lisp-mode . parinfer-mode)
  ;;          (scheme-mode . parinfer-mode)
  ;;          (lisp-mode . parinfer-mode))
  ;;   :config
  ;;   (setq parinfer-extensions
  ;;         '(defaults       ; should be included.
  ;;            pretty-parens  ; different paren styles for different modes.
  ;;            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
  ;;            smart-yank)))  ; Yank behavior depend on mode.
  )
(provide 'wmad-devel-pkgs)
;;; wmad-devel-pkgs.el ends here
