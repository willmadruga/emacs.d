;; wmad-dired-config.el --- Dired Configuration Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/dired-config ()
  "Dired configuration."
  
  ;; Dired configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (dired-listing-switches "-agho --group-directories-first")

  ;; dired single window ;;;;;;;;;;;;;
  (use-package dired-single)

  ;; All the icons for dired ;;;;;;;;;;;;;;;;;;;;;
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

  ;; dired open extensions-binary map ;;;;;;;;;;;;;;;;
  (use-package dired-open
    :config
    (setq dired-open-extensions '(("png" . "feh")
                                  ("mkv" . "mpv")
                                  ("mp3" . "mpv")
                                  ("pdf" . "acroread")
                                  )))

  ;; dired hide dotfiles ;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode))

  ;; dired sidebar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package dired-sidebar
    :commands (dired-sidebar-toggle-sidebar)
    :config
    (setq dired-sidebar-theme 'icons)
    (setq dired-sidebar-refresh-on-projectile-switch t)
    (setq dired-sidebar-should-follow-file t)
    (setq dired-sidebar-one-instance-p t))

  ;; https://github.com/crocket/dired-single/tree/98c2102429fcac6fbfdba9198c126eb1b3dcc4e5
  (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
    ;; <add other stuff here>
    (define-key dired-mode-map [remap dired-find-file]
      'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
      'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory]
      'dired-single-up-directory))

  (use-package emacs
    :config
    ;; if dired's already loaded, then the keymap will be bound
    (if (boundp 'dired-mode-map)
        ;; we're good to go; just add our bindings
        (my-dired-init)
      ;; it's not loaded yet, so add our bindings to the load-hook
      (add-hook 'dired-load-hook 'my-dired-init)))

  )

(provide 'wmad-dired-config)

;;; wwmad-dired-config.el ends here
