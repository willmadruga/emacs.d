;;; init.el --- Initialization File -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; load the various configuration files
(load-file (concat user-emacs-directory "wmad/core/load-paths.el"))
(require 'wmad-core)

(wmad/pre-init)
(wmad/init)
(wmad/post-init)

;;; init.el ends here
