;; load-paths.el --- Core configuration: load paths

(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defvar start-directory user-emacs-directory)

(defconst core-directory (expand-file-name (concat start-directory "wmad/core")))

(defconst layers-directory (expand-file-name (concat start-directory "wmad/layers")))

(defconst devel-directory (expand-file-name (concat start-directory "wmad/layers/devel")))

(defconst org-directory (expand-file-name (concat start-directory "wmad/layers/org")))

(defconst elisp-directory (expand-file-name (concat start-directory "elisp/")))

(mapc 'add-to-load-path
      `(
        ,core-directory
	,layers-directory
	,devel-directory
	,org-directory
        ,elisp-directory
        ))

;;; load-paths.el ends here
