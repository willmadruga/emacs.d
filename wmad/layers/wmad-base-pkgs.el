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
    (company-minimum-prefix-length 3)
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
  (defun wmad-configure-navigator ()
    "Configure the dashboard navigator."
    (setq dashboard-navigator-buttons
          `((
             (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
              "emacs.d" "" (lambda (&rest _) (browse-url "https://github.com/wmadruga/emacs.d")))

             ("" "Restart" "" (lambda (&rest _) (restart-emacs)))

             (,(all-the-icons-faicon "calendar" :height 1.1 :v-adjust 0.0)
              "TODO" "" (lambda (&rest _) (wmad/open-todo)))

             (,(all-the-icons-faicon "book" :height 1.1 :v-adjust 0.0)
              "Journal" "" (lambda (&rest _) (wmad/open-journal)))

             ))))

  (use-package dashboard
    :config
    (setq dashboard-items
          '((recents . 15)
            (projects . 7)
            (bookmarks . 3)
            (agenda . 20)))

    (setq dashboard-set-init-info t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)

    (setq dashboard-set-navigator t)
    (wmad-configure-navigator)

    ;; (setq dashboard-startup-banner 'logo)
    (setq dashboard-startup-banner (concat user-emacs-directory "/wmad/resources/gameoflife.png"))

    (dashboard-modify-heading-icons '((recents . "file-text")
                                      (bookmarks . "bookmark")))
    (dashboard-setup-startup-hook))

  ;; whitespace butler ;;;;;;;;;;;;;;;;;;;
  (use-package ws-butler
    :hook ((text-mode . ws-butler-mode)
           (prog-mode . ws-butler-mode)))

  (use-package ido
    :init (ido-mode 1)
    :config
    (setq ido-enable-flex-matching nil)
    (setq ido-create-new-buffer 'always)
    (setq ido-everywhere t))

  (use-package ido-vertical-mode
    :ensure t
    :init
    (ido-vertical-mode 1)
    :config
    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

  ;; (use-package firestarter) ;; https://depp.brause.cc/firestarter/

  )

(provide 'wmad-base-pkgs)
;;; wmad-base-pkgs.el ends here
