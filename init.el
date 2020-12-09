;;; init.el --- Initialization File

;;; Commentary:

;;; Code:

;; load all paths for the various configuration files
(load-file (concat user-emacs-directory "wmad/core/load-paths.el"))
(require 'wmad-core)
(wmad/init)

;; (require 'server)
;; (unless (server-running-p) (server-start))


;;; init.el ends here
