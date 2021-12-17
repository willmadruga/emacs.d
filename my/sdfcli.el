;;; sdfcli.el --- SuiteCloud Developer Framework client wrapper -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is a work in progress!
;; Depends on Netsuite's sdfcli java
;; Testing with suitecloud node cli is on hold at the moment...

;;; Code:

(require 'dash)

(defcustom netsuite-sdfcli-sync-to-account nil
  "Netsuite SDFCLI Auto Sync to account."
  :type 'sdfcli-authid
  :group 'netsuite
  )

;; Work in Progress... testing using suitecloud (node cli)
;; (defun netsuite/setup-account ()
;;   "Account setup."
;;   (interactive)
;;   (let ((project-path (car (split-string (buffer-file-name) "FileCabinet"))))
;;     (async-shell-command (concat "cd " project-path " && suitecloud account:setup"))))

(defun netsuite/list-authids ()
  "An sdfcli wrapper to list registered authids."
  (let* ((sdfcli-command "sdfcli manageauth -list")
         (sdfcli-result  (shell-command-to-string sdfcli-command))
         (sdfcli-options (cdr (split-string sdfcli-result "\n"))))
    (-non-nil (mapcar (lambda (v) (car (split-string v))) sdfcli-options))))

(defun netsuite/deploy21 ()
  "An sdfcli wrapper to deploy current project.  Project is identified based on buffer's file path."

  (interactive)
  (let ((project-path (car (split-string (buffer-file-name) "FileCabinet")))
        (sdf-authid (ido-completing-read "SDF Auth id:" (netsuite/list-authids))))
    (async-shell-command (concat "sdfcli21 deploy -applyinstallprefs -authid " sdf-authid " -p " project-path))))

(defun netsuite/deploy ()
  "An sdfcli wrapper to deploy current project.  Project is identified based on buffer's file path."

  (interactive)
  (let ((project-path (car (split-string (buffer-file-name) "FileCabinet")))
        (sdf-authid (ido-completing-read "SDF Auth id:" (netsuite/list-authids))))
    (async-shell-command (concat "sdfcli deploy -sw -np -authid " sdf-authid " -p " project-path))))

(defun netsuite/create-project ()
  "An sdfcli wrapper to create a new SDF project."

  (interactive)
  (let* ((types '("SUITEAPP" "ACCOUNTCUSTOMIZATION"))
         (project-type (ido-completing-read "Project Type:" types))
         (project-id (read-string "Project Id:"))
         (project-name (read-string "Project Name:"))
         (project-version (read-string "Project Version:"))
         (project-parent-dir (read-directory-name "Parent Directory:"))
         (publisher-id (read-string "Publisher Id:"))
         (create-project-command (concat "sdfcli createproject -type " project-type
                                         " -parentdirectory " project-parent-dir
                                         " -projectname " project-name
                                         " -projectid " project-id
                                         " -publisherid " publisher-id
                                         " -projectversion " project-version)))
    (message (shell-command-to-string create-project-command))))

(defun netsuite/upload-buffer ()
  "An sdfcli wrapper to upload the current buffer to a netsuite account."

  (interactive)
  (let ((project-path (car (split-string (buffer-file-name) "FileCabinet")))
        (file-path    (car (cdr (split-string (buffer-file-name) "FileCabinet"))))
        (sdf-authid   (progn
                        (if (bound-and-true-p netsuite-sdfcli-sync-to-account)
                            netsuite-sdfcli-sync-to-account
                          (ido-completing-read "SDF Auth id:" (netsuite/list-authids))))))
    (async-shell-command (concat "sdfcli uploadfiles -paths " file-path  " -authid " sdf-authid " -p " project-path))))

(defun netsuite/upload-buffer21 ()
  "An sdfcli wrapper to upload the current buffer to a netsuite account."

  (interactive)
  (let ((project-path (car (split-string (buffer-file-name) "FileCabinet")))
        (file-path (car (cdr (split-string (buffer-file-name) "FileCabinet"))))
        (sdf-authid (ido-completing-read "SDF Auth id:" (netsuite/list-authids))))
    (async-shell-command (concat "sdfcli21 uploadfiles -paths " file-path  " -authid " sdf-authid " -p " project-path))))

(defun netsuite/is-suiteapp ()
  "Whether the buffer belongs to a SuiteAPP."
  (car (split-string (buffer-file-name) "FileCabinet"))
  )

(defun netsuite/sync-toggle ()
  "Toggle buffer/project sync with account."
  (interactive)
  (if (bound-and-true-p netsuite-sdfcli-sync-to-account)
      (progn
        (setq netsuite-sdfcli-sync-to-account nil)
        (remove-hook 'after-save-hook 'netsuite/upload-buffer)
        (message "Turned OFF Netsuite SuiteCloud Auto-Sync to account."))
    (progn
      (setq netsuite-sdfcli-sync-to-account (ido-completing-read "SDF Auth id:" (netsuite/list-authids)))
      (add-hook 'after-save-hook 'netsuite/upload-buffer)
      (message (concat "Turned ON Netsuite SuiteCloud Auto-Sync to account: " netsuite-sdfcli-sync-to-account))))

  ;; TODO:
  ;; continue work on is-suiteapp() to find if buffer belongs to a suiteapp.
  ;; create a macro that executes the @body only if is-suiteapp returns true (kinda like with-package macro)
  ;; hook the macro to after-save-buffer so that we only upload to netsuite actual netsuite JS files...
  ;;
  ;; 2. WHEN I SAVE A .JS BUFFER, CALL NETSUITE UPLOAD
  ;; 3. WHEN I SAVE A .XML BUFFER, CALL NETSUITE DEPLOY
  )

(provide 'sdfcli)

;;; sdfcli.el ends here
