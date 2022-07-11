;; rational-addon-suitecloud.el --- Rational Addon for suitecloud  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: William Madruga

;; Commentary

;; SuiteCloud Developer Framework
;; https://github.com/oracle/netsuite-suitecloud-sdk/tree/master/packages/node-cli
;;

;;; Code:

(require 'dash)

(defcustom netsuite-sdfcli-sync-to-account nil
  "Netsuite SDFCLI Auto Sync to account."
  :type 'sdfcli-authid
  :group 'netsuite)

(defvar suitecloud "suitecloud "
  nil)

;; FIXME...
;; for now just pop up a terminal an run the command...
(defun suitecloud/account-setup ()
  "Suitecloud account setup wrapper."
  (interactive)
  (let ((project-path (car (split-string (buffer-file-name) "FileCabinet"))))
    (shell-command (concat "cd " project-path " && " suitecloud " account:setup -i"))))

(defun suitecloud/deploy ()
  "An sdfcli wrapper to deploy current project.  Project is identified based on buffer's file path."
  (interactive)
  (let ((project-path (car (split-string (buffer-file-name) "FileCabinet"))))
    (async-shell-command (concat "cd " project-path " && " suitecloud "project:deploy"))))

(defun suitecloud/upload-file ()
  "An sdfcli wrapper to upload the current buffer to a netsuite account."
  (interactive)
  (let ((project-path (car (split-string (buffer-file-name) "FileCabinet")))
        (file-path    (car (cdr (split-string (buffer-file-name) "FileCabinet")))))
    (async-shell-command (concat "cd " project-path " && " suitecloud "file:upload --paths "
                                 file-path))))

(provide 'rational-addon-suitecloud)
;;; rational-addon-suitecloud.el ends here
