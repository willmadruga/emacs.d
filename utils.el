;;; utils.el --- Utils ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

(defun wmad/package-install (pname)
  "Install package PNAME if missing."
  (require 'package)
  (if (not (require pname  nil 'noerror))
      (progn
        (package-refresh-contents)
        (package-install pname))))

(defmacro wmad/if-package (package &rest body)
  "Execute BODY if PACKAGE is installed."
  (declare (indent 1))
  `(and (require ,package nil 'noerror)
        (progn ,@body)))

(defun immortal-scratch ()
  "Enable immortal scratch buffer."
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil)
    t))

(defun save-persistent-scratch ()
  "Save the contents of *scratch*."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max)
                  (concat user-emacs-directory "/var/scratch"))))

(defun load-persistent-scratch ()
  "Reload the scratch buffer."
  (let ((scratch-file (concat user-emacs-directory "/var/scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file)))))

;; Interactive Supporting Functions
(defun wmad/eslint-fix-buffer-file ()
  "Use eslint to fix buffer file."
  (interactive)
  (async-shell-command (concat "eslint --fix " (buffer-file-name))))

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
