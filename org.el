;;; org.el --- ORG related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-agenda)
(require 'appt)

(if (file-exists-p "~/src/2nd_brain/")
    (setq org-directory "~/src/2nd_brain/")
  (setq org-directory "~/"))

(setq org-adapt-indentation nil)
(setq org-ellipsis " ▼ ")
(setq org-hide-emphasis-markers t)
(setq org-todo-keywords
      '((sequence
	       "TODO(t)"
	       "NEXT(n)"
	       "STRT(s)"
	       "WAIT(x)"
	       "|"
	       "DONE(d)"
	       "CANCELLED(c)")))

;; org capture
(if (file-exists-p "~/src/2nd_brain/journal.org")
    (defvar +org-capture-journal-file "~/src/2nd_brain/journal.org")
  (defvar +org-capture-journal-file "~/journal.org"))

(setq org-capture-templates
	    '(("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %?\n%i\n%a" :prepend t :jump-to-captured t)))

;; org-agenda
(if (file-exists-p "~/src/2nd_brain/brain")
    (setq org-agenda-files '("~/src/2nd_brain/brain/TODO-LIST.org" "~/src/2nd_brain/brain/Finances.org")))

(setq org-agenda-start-day "0d")
(setq org-agenda-span 5)
(setq org-agenda-include-diary t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-use-time-grid t)
(setq appt-display-duration 60)

(wmad/package-install 'org-brain)
(require 'org-brain)

(if (file-exists-p "~/src/2nd_brain/brain")
    (setq org-brain-path "~/src/2nd_brain/brain"
          org-id-locations-file "~/src/2nd_brain/brain/.orgids"))

(setq org-id-locations-file-relative t)
(setq org-brain-visualize-default-choices 'all)
(setq org-brain-title-max-length 12)
(setq org-brain-include-file-entries nil)
(setq org-brain-file-entries-use-title nil)

(add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
(push '("b" "Brain" plain (function org-brain-goto-end)
	      "* %i%?" :empty-lines 1)
	    org-capture-templates)

(wmad/package-install 'org-superstar)

;; think this can be improved, right?
;; (wmad/package-install 'org-bullets)
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; org.el ends here
