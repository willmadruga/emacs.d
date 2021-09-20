;;; ui-conf.el --- UI config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mode-line '3d)
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

;; https://www.gonsie.com/blorg/modeline.html
;; (setq-default mode-line-format
;;               (list
;;                ;; day and time
;;                '(:eval
;;                  (propertize (format-time-string " %b %d %H:%M ")
;;                              'face 'font-lock-builtin-face))

;;                ;; version control
;;                '(:eval
;;                  (if (not vc-mode)
;;                      ""
;;                    (propertize (substring vc-mode 5)
;;                                'face 'font-lock-constant-face)))

;;                ;; uffer name; the file name as a tool tip
;;                '(:eval
;;                  (propertize " %b "
;;                              'face
;;                              (let ((face (buffer-modified-p)))
;;                                (if face 'font-lock-warning-face
;;                                  'font-lock-type-face))
;;                              'help-echo (buffer-file-name)))

;;                ;; line and column
;;                ;; '%02' to set to 2 chars at least; prevents flickering
;;                " ("
;;                (propertize "%02l" 'face 'font-lock-keyword-face) ","
;;                (propertize "%02c" 'face 'font-lock-keyword-face)
;;                ") "

;;                ;; relative position
;;                "[" (propertize "%p" 'face 'font-lock-constant-face) "] "

;;                ;; mode-line-modes

;;                mode-line-misc-info

;;                ;; spaces to align right
;;                '(:eval
;;                  (propertize
;;                   " " 'display
;;                   `((space :align-to (- (+ right right-fringe right-margin)
;;                                         ,(+ (string-width org-mode-line-string) (+ 3 (string-width mode-name)))
;;                                         )))))

;;                (propertize org-mode-line-string 'face '(:foreground "#5DD8FF"))

;;                ;; the current major mode
;;                (propertize " %m " 'face 'font-lock-string-face)
;;                ))

(use-package emacs
  :config
  (set-face-attribute 'mode-line nil
                      :background "#353644"
                      :foreground "white"
                      :box '(:line-width 8 :color "#353644")
                      :overline nil
                      :underline nil)

  (set-face-attribute 'mode-line-inactive nil
                      :background "#565063"
                      :foreground "white"
                      :box '(:line-width 8 :color "#565063")
                      :overline nil
                      :underline nil)

  (set-face-attribute 'default        nil :font "DejaVu Sans Mono" :height 100)
  (set-face-attribute 'fixed-pitch    nil :font "DejaVu Sans Mono")
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans")
)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; ui-conf.el ends here
