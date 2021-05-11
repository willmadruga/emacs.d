;;; clean.el --- Maintain a clean emacs folder ;; -*- lexical-binding: t; -*-

;;; Commentary:

;; I use no-littering package to keep my emacs.d clean.
;; This file contain minor fixes for a cleaner experience, specially when loading for the first time

;;; Code:

;; Create /etc and /var if non existent
(dolist (c '(
             "/var"
             "/etc"
             ))
  (let ((d (concat user-emacs-directory c)))
    (when (not (file-exists-p d)) (make-directory d))))

(wmad/package-install 'no-littering)
(require 'no-littering)

(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(add-to-list 'recentf-exclude (concat user-emacs-directory "elpa"))

(recentf-mode 1)
(setq auto-save-file-name-transforms
	    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backups"))))

;; Remove files/directories I know should be inside no-littering directory, but gets created before it's enabled.
(if (file-exists-p (concat user-emacs-directory "/transient"))
    (delete-directory (concat user-emacs-directory "/transient") t t))
(if (file-exists-p (concat user-emacs-directory "/history"))
    (delete-file (concat user-emacs-directory "/history") t))

;;; clean.el ends here
