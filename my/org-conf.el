;;; org-conf.el --- ORG-Mode config ;; -*- lexical-binding: t; -*-
;; This file is NOT part of GNU Emacs.
;;; Commentary:
;;; Code:

(setup org
  (require 'org-clock)
  (setq org-return-follows-link t)
  (setq org-adapt-indentation t)
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width '(300))
  (setq org-hide-emphasis-markers t)
  (setq org-ellipsis " ▼ ")

  (setq org-src-preserve-indentation nil)
  (setq org-edit-src-content-indentation 2)
  (setq org-src-window-setup 'current-window)

  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-timer-display 'both)
  (setq org-clock-sound (concat user-emacs-directory "alert.wav"))

  (setq org-todo-keywords
        '((sequence
           ">"
           "TODO(t)"
           "NEXT(n)"
           "STRT(s)"
           "WAIT(x)"
           "|"
           "CANCELLED(c)"
           "DONE(d)"))))

(setup (:package org-roam)

  (setq org-roam-v2-ack t)
  (require 'org-roam-dailies)

  (setq org-roam-directory "~/src/org-roam")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-db-autosync-mode)

  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))

(setup org-agenda
  (require 'appt)
  (require 'org-agenda)
  (require 'org-id)
  (if (file-exists-p "~/src/org-roam")
      (setq org-agenda-files
            '(
              "~/src/org-roam/202101_finances.org"
              "~/src/org-roam/202102_finances.org"
              "~/src/org-roam/202103_finances.org"
              "~/src/org-roam/202104_finances.org"
              "~/src/org-roam/202105_finances.org"
              "~/src/org-roam/202106_finances.org"
              "~/src/org-roam/202107_finances.org"
              "~/src/org-roam/202108_finances.org"
              "~/src/org-roam/202109_finances.org"
              "~/src/org-roam/202110_finances.org"
              "~/src/org-roam/202111_finances.org"
              "~/src/org-roam/202112_finances.org"
              "~/src/org-roam/20210824165844-todo.org")))

  (setq org-agenda-start-day "0d")
  (setq org-agenda-span 5)
  (setq org-agenda-include-diary t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-use-time-grid t)
  (setq appt-display-duration 60)
  (setq org-id-locations-file-relative t))

(setup (:package org-superstar)
  (:hook-into org-mode)

  (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 140)
  (setq org-superstar-headline-bullets-list '(" "))
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-leading-bullet "")
  (setq org-superstar-todo-bullet-alist '(
                                          ("TODO" . ?⌖)
                                          ("NEXT" . ?)
                                          ("STRT" . ?✍)
                                          ("WAIT" . ?)
                                          ("CANCELLED" . ?)
                                          ("DONE" . ?✔)
                                          (">" . ?)
                                          )))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; org-conf.el ends here
