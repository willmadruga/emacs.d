;;; init.el --- Initialization File -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad-pre-init ()
  "Pre-Initialization tasks."

  ;; load the various configuration files
  (load-file (concat user-emacs-directory "wmad/core/load-paths.el"))

  ;; Increase GC threshold
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

  ;; temporarily unset file-name-handler-alist
  (defvar wmad--file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil))

(defun wmad-post-init ()
  "Post-Initialization tasks."

  ;; after startup, it is important you reset the GC back to some reasonable default.
  ;; A large gc-cons-threshold will cause freezing and stuttering during long-term
  ;; interactive use. I find these are nice defaults:
  (add-hook
   'emacs-startup-hook
   (lambda ()
     ;; decrease GC threshold
     (setq gc-cons-threshold 16777216 ; 16mb
           gc-cons-percentage 0.1)
     ;; restore file-name-handler-alist
     (setq file-name-handler-alist wmad--file-name-handler-alist)

     ;; wmad: a workaround dashboard's workaround to prevent flooding the recent files list with org mode files
     (recentf-load-list)

     ;; Profile start-up
     (message
      "Emacs ready in %s with %d garbage collections."
      (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done)
     )))

(wmad-pre-init)

(require 'wmad-core)
(wmad/init)

(wmad-post-init)

;;; init.el ends here
