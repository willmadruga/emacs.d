;;; init.el --- Initialization File
;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; load all paths for the various configuration files
(load-file (concat user-emacs-directory "wmad/core/load-paths.el"))
(require 'wmad-core)

(wmad/pre-init)
(wmad/init)
(wmad/post-init)

;; TODO:
;; (require 'server)
;; (unless (server-running-p) (server-start))


;;; init.el ends here
