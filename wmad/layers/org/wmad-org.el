;; wmad-org.el --- Org-Mode Configuration Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/org-config ()
  "Org-Mode configuration."
  
  (require 'org-indent)
  (require 'org-tempo)
  (require 'org-habit)
  
  (straight-use-package '(org :type built-in))

  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (add-hook 'org-mode-hook (lambda ()
                             (org-indent-mode 1)
                             (visual-line-mode 1)
                             (variable-pitch-mode 1)
                             (auto-fill-mode 0)
                             (wmad/org-mode-visual-fill)))

  (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))

  ;; Workflow
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "STRT(s)"  "WAIT(w)"  "|" "DONE(d!)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "orange red" :weight bold))
          ("NEXT" . (:foreground "yellow" :weight bold))
          ("STRT" . (:foreground "green" :weight bold))
          ("WAIT" . (:foreground "MediumPurple3" :weight bold))
          ("DONE" . (:foreground "blue" :weight bold))))


  ;; Org Customizations
  ;; font setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (dolist (face '((org-level-1 . (1.7 . "#e76f51"))
                  (org-level-2 . (1.5 . "#f4a261"))
                  (org-level-3 . (1.3 . "#e9c46a"))
                  (org-level-4 . (1.1 . "#2a9d8f"))
                  (org-level-5 . (1.0 . "#e76f51"))
                  (org-level-6 . (1.0 . "#f4a261"))
                  (org-level-7 . (1.0 . "#e9c46a"))
                  (org-level-8 . (1.0 . "#2a9d8f"))))

    (set-face-attribute (car face) nil
			:font "Roboto Mono Light"
			:weight 'regular
			:height (cadr face)
			:foreground (cddr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Org Capture
  (defvar +org-capture-journal-file "~/src/2nd_brain/journal.org")
  (setq org-capture-templates
        '(("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)))

  ;; org superstar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package org-superstar
	       :after org
	       :config
	       (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

  ;; org super-agenda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package org-super-agenda
	       :after org-agenda
	       :config
	       (org-super-agenda-mode)
	       (let ((org-super-agenda-groups
		      '((:auto-group t)))))

	       (setq org-agenda-window-setup 'current-window)

	       ;; Location of agenda files
	       (setq org-agenda-files (concat user-emacs-directory "elisp/agenda-files.el")))
  )

(defun wmad/org-mode-visual-fill ()
  (setq visual-fill-column-width 200
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(provide 'wmad-org)

;;; wmad-org.el ends here
