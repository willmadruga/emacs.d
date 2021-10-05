;;; exwm-conf.el --- EXWM ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Thanks David Wilson! :claps:
;; https://github.com/daviwil/dotfiles/blob/master/Desktop.org

;;; Code:

;; PACKAGE LOAD/INSTALL~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(add-to-list 'load-path "~/.exwm/xelb")
(add-to-list 'load-path "~/.exwm/exwm") ;; my fork so I can attempt to fix issues I find along the way.
(require 'exwm)

(dolist (pname
         '(exwm-edit desktop-environment))
  (unless (package-installed-p pname)
    (progn
      (package-refresh-contents)
      (package-install pname))))

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
(setq mouse-autoselect-window nil)
(setq focus-follows-mouse t)
(setq exwm-workspace-warp-cursor t)
(setq exwm-workspace-number 1)
(setq exwm-manage-force-tiling t)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(require 'all-the-icons)
(add-hook 'exwm-update-title-hook
          (lambda ()
            (or
             (pcase exwm-class-name
               ("qutebrowser" (exwm-workspace-rename-buffer (format " %s" exwm-title)))
               ("Signal" (exwm-workspace-rename-buffer      (format " %s" exwm-title)))
               ("Slack" (exwm-workspace-rename-buffer       (format " %s" exwm-title)))
               ("mpv" (exwm-workspace-rename-buffer         (format " %s" exwm-title)))
               ("zoom" (exwm-workspace-rename-buffer        (format " %s" exwm-title))))
             (exwm-workspace-rename-buffer (format "%s %s" (all-the-icons-icon-for-buffer) (buffer-name))))))

(require 'exwm-edit)

;; EXWM BINDINGS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Remove keybindings for 'delete-other-windows because I don't want to accidentally (force of habit) call it
(global-unset-key (kbd "C-x 1"))

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
        ?\M-\     ;; Alt+Space
        ?\C-\;))

(exwm/bind-function
 "s-<return>"              (lambda () (exwm/run-in-background "alacritty"))
 "s-&"                     (lambda (command)
                             (interactive (list (read-shell-command "$ ")))
                             (start-process-shell-command command nil command))

 "C-c w k"                 'kill-buffer
 "C-c w R"                 'exwm-restart

 "<print>"                 'xfce4-screenshooter
 "<XF86AudioLowerVolume>"  'desktop-environment-volume-decrement
 "<XF86AudioRaiseVolume>"  'desktop-environment-volume-increment
 "<XF86AudioMute>"         'desktop-environment-toggle-mute)

;; Ctrl+Q enable the next key to be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Set up global key bindings.  These always work, no matter the input state!
;; Keep in mind that changing this list after EXWM initializes has no effect.
(setq exwm-input-global-keys
      `(
        ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        ([?\s-r] . exwm-reset)

        ;; Move between windows
        ([s-left]  . windmove-left)
        ([s-right] . windmove-right)
        ([s-up]    . windmove-up)
        ([s-down]  . windmove-down)

        ;; Launch applications via shell command
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

        ;; Switch workspace
        ([?\s-w] . exwm-workspace-switch)
        ;; ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

        ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 1 9))))


;; enable exwm before the first frame is displayed.
(exwm-enable)

;; We land on workspace 1 at startup
(exwm-workspace-switch-create 1)

;; Launch apps a couple of apps - Ignore errors when binaries are not found.
(ignore-errors
  (exwm/run-in-background "nm-applet"))
(ignore-errors
  (exwm/run-in-background "blueberry-tray"))

(ignore-errors
  (start-process-shell-command "Signal" nil "flatpak run org.signal.Signal"))
(ignore-errors
  (start-process-shell-command "Zoom" nil "zoom"))
(ignore-errors
  (start-process-shell-command "Slack" nil "slack"))

;; PANEL  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

;; TODO: work on an alternative panel. I don't want to rely on XFCE for too long.
;; (ignore-errors
;;   (start-process-shell-command "xfce4-panel" nil "xfce4-panel"))

(provide 'exwm-config)

;;; exwm-conf.el ends here
