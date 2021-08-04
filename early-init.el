;;; early-init.el --- Emacs early init ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; To keep Emacs from automatically making packages available at
;; startup, change the variable ‘package-enable-at-startup’ to ‘nil’.  You
;; must do this in the early init file, as the variable is read before
;; loading the regular init file.
(setq package-enable-at-startup nil)

;; https://gitlab.com/nathanfurnal/dotemacs/-/blob/master/init.el
;; Speed up startup
;; High garbage collection at startup needs to be reset at some point then we defer the work to `gcmh'.

;; https://www.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/
;; changing GC in early-init instead of init.
(setq gc-cons-threshold 16777216 ; 16mb
      gc-cons-percentage 0.1)



;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-buffer-menu t)
(setq inhibit-splash-screen       t)
(setq inhibit-startup-screen      t)
(setq use-dialog-box              t)
(setq use-file-dialog           nil)

;;; early-init.el ends here
