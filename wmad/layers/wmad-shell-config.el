;; wmad-shell-config.el --- Emacs Configuration Layer: Shell  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/shell-config ()
  "Shell configuration."

  ;; Inferior Shell Mode
  (use-package shell
    :config
    (setq explicit-shell-file-name "/usr/bin/zsh")
    (setq shell-file-name "zsh")
    (setenv "SHELL" shell-file-name)
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

  ;; vterm ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package vterm
    :commands vterm
    :config
    (setq vterm-shell "zsh")
    (setq vterm-max-scrollback 10000))

  )

(provide 'wmad-shell-config)
;;; wmad-shell-config.el ends here
