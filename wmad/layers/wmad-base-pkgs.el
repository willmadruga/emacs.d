;; wmad-base-pkgs.el --- Emacs Configuration Layer: Base packages -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/base-packages-init ()
  "Base packages configuration."
  ;; diminish ;;;;;;;;;
  (use-package diminish
    :after use-package)

  ;; Bufler ;;;;;;;;;;;;;;;;;;;
  (use-package bufler
    :bind (("C-x b" . bufler)))

  ;; no-littering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package no-littering
    :config
    (require 'recentf)
    (defvar recentf-exclude)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)

    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


  ;; Prescient ;;;;;;;;;;
  (use-package prescient)
  ;; Selectrum, Selectrum-Prescient ;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package selectrum
    :config
    (selectrum-mode 1))

  (use-package selectrum-prescient
    :config
    ;; to make sorting and filtering more intelligent
    (selectrum-prescient-mode 1)
    ;; to save your command history on disk, so the sorting gets more
    ;; intelligent over time
    (prescient-persist-mode 1))

  ;; undo-fu ;;;;;;;;;
  (use-package undo-fu)
  
  ;; which-key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-show-early-on-C-h t)
    (setq which-key-idle-delay 10000)
    (setq which-key-idle-secondary-delay 0.05))

  ;; try ;;;;;;;;;
  (use-package try)
  ;; restart-emacs ;;;;;;;;;
  (use-package restart-emacs)

  ;; helpful ;;;;;;;;;
  (use-package helpful)

  ;; Company ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package company
    :after lsp-mode
    :bind (:map company-active-map
                ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))

  (add-hook 'after-init-hook 'global-company-mode)

  ;; company-box ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package company-box
    :hook (company-mode . company-box-mode))

  (use-package company-prescient
    :after company
    :config (company-prescient-mode 1))
    
  ;; vterm ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package vterm
    :commands vterm
    :config
    (setq vterm-shell "zsh")
    (setq vterm-max-scrollback 10000))

  ;; Dashboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package dashboard
    :config
    (setq dashboard-items
          '((recents . 15)
            (projects . 5)
            (bookmarks . 5)
            (agenda . 20)))
    (setq dashboard-set-init-info t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (dashboard-modify-heading-icons '((recents . "file-text")
                                      (bookmarks . "book")))
    (dashboard-setup-startup-hook))

  ;; whitespace butler ;;;;;;;;;;;;;;;;;;;
  (use-package ws-butler
    :hook ((text-mode . ws-butler-mode)
           (prog-mode . ws-butler-mode)))
  
  )

(provide 'wmad-base-pkgs)
;;; wmad-base-pkgs.el ends here
