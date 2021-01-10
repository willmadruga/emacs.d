;; load-paths.el --- Core configuration: load paths -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun add-to-load-path (dir)
  "Add directory DIR to 'load-path'."
  (add-to-list 'load-path dir))

(defvar start-directory user-emacs-directory)

(defconst core-directory (expand-file-name (concat start-directory "wmad/core")))

(defconst layers-directory (expand-file-name (concat start-directory "wmad/layers")))

(defconst devel-directory (expand-file-name (concat start-directory "wmad/layers/devel")))

(defconst org-directory (expand-file-name (concat start-directory "wmad/layers/org")))

(defconst fun-directory (expand-file-name (concat start-directory "wmad/layers/fun")))

(defconst elisp-directory (expand-file-name (concat start-directory "elisp/")))

;; Dependent modules needs to be loaded first to suppress 'flycheck' errors.
(mapc 'add-to-load-path
      `(
	      ,fun-directory
	      ,devel-directory
	      ,org-directory
	      ,layers-directory
        ,core-directory
        ,elisp-directory
        ))

;;; load-paths.el ends here
