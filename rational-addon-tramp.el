;;; rational-addon-tramp.el --- Tramp config         -*- lexical-binding: t; -*-

(require 'tramp)

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

(add-to-list 'tramp-remote-path "~/.nvm/versions/node/v18.5.0/bin/")

(setq tramp-default-method "ssh")
(setq tramp-default-remote-shell "/bin/bash")
(setq explicit-shell-file-name "bash")
(setq tramp-auto-save-directory temporary-file-directory)
(setq tramp-verbose 10)

(provide 'rational-addon-tramp)
