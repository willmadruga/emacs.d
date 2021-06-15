;;; utils.el --- Utils ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/package-install (pname)
  "Install package PNAME, if missing, and add it to package-selected-packages."
  (require 'package)
  (if (not (require pname  nil 'noerror))
      (progn
        (package-refresh-contents)
        (package-install pname))))

(defmacro wmad/if-package (package &rest body)
  "Execute BODY if PACKAGE is installed."
  (require 'package)
  (declare (indent 1))
  `(and (require ,package nil 'noerror)
        (progn ,@body)))

(defun immortal-scratch ()
  "Enable immortal scratch buffer."
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil)
    t))

;; Interactive Supporting Functions
(defun wmad/eslint-fix-buffer-file ()
  "Use eslint to fix buffer file."
  (interactive)
  (async-shell-command (concat "/home/wmadruga/.npm-packages/bin/eslint --fix " (buffer-file-name))))

(defun wmad/duplicate-line ()
  "Duplicates line at point."
  (interactive)
  (let* ((cursor-column (current-column)))
    (move-beginning-of-line 1)
    (kill-line) (yank) (newline) (yank)
    (move-to-column cursor-column)))

(defun wmad/update-my-orgids ()
  "Forcefully update orgids for files I care about."
  (interactive)
  (org-id-update-id-locations (directory-files "~/src/2nd_brain/brain")))

(defun wmad/open-dashboard ()
  "Open dashboard buffer."
  (interactive)
  (switch-to-buffer "*dashboard*" nil t))


(provide 'utils)
;;; utils.el ends here
