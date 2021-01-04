;; pkgsys.el --- Core configuration : Package System -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad-pkgsys-init ()
  "Package System configuration initialization."
  
  (defvar bootstrap-version)

  (let ((bootstrap-file
	       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	      (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	      (goto-char (point-max))
	      (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (setq straight-use-package-by-default t)

  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)
  
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  )

(provide 'wmad-pkgsys)

;;; wmad-pkgsys.el ends here
