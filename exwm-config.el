;;; exwm-config.el --- EXWM ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Thanks David Wilson! :claps:
;; https://github.com/daviwil/dotfiles/blob/master/Desktop.org

;;; Code:

;; HELPER FUNCTIONS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun exwm/run-in-background (command)
  "Run COMMAND in background."
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm/bind-function (key invocation &rest bindings)
  "Globally create KEY BINDINGS for function INVOCATION."
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (funcall ',invocation)))
    (setq key (pop bindings) invocation (pop bindings))))

(defun exwm/bind-command (key command &rest bindings)
  "Globally create KEY BINDINGS for COMMAND."
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (exwm/run-in-background ,command)))
    (setq key (pop bindings) command (pop bindings))))

;; EXWM CONFIG  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'exwm)
(setq mouse-autoselect-window nil)
(setq focus-follows-mouse t)
(setq exwm-workspace-warp-cursor t)
(setq exwm-workspace-number 5)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(add-hook 'exwm-update-title-hook
          (lambda ()
            (pcase exwm-class-name
              ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title))))))

;; turn it on
(exwm-enable)

;; Make workspace 1 be the one where we land at startup
(exwm-workspace-switch-create 1)

;; Launch apps that will run in the background
(exwm/run-in-background "flatpak run org.signal.Signal")
(exwm/run-in-background "zoom")
(exwm/run-in-background "slack")
(exwm/run-in-background "qutebrowser")
(exwm/run-in-background "nm-applet")
(exwm/run-in-background "blueberry-tray")

;; EXWM BINDINGS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\M-k  ;; Browser list
      ?\C-\M-n  ;; Next workspace
      ?\C-\     ;; Ctrl+Space
      ?\C-\;))

;; Ctrl+Q will enable the next key to be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(exwm/bind-function
 "s-<up>"                  'windmove-up
 "s-<down>"                'windmove-down
 "s-<left>"                'windmove-left
 "s-<right>"               'windmove-right
 "s-q"                     'kill-buffer
 "s-S-e"                   'exwm-restart
 "<print>"                 'desktop-environment-screenshot
 "<XF86AudioLowerVolume>"  'desktop-environment-volume-decrement
 "<XF86AudioRaiseVolume>"  'desktop-environment-volume-increment
 "<XF86AudioMute>"         'desktop-environment-toggle-mute
 )

;; TODO:
;; multiple workspaces
;; switch workspaces with super+number
;; move window to another workspace with shift+super+number

;; PANEL  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(async-shell-command "xfce4-panel")

(provide 'exwm-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; exwm-config.el ends here
