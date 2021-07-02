;;; bindings.el --- Bindings related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(wmad/package-install 'general)
(require 'general)

;; (global-unset-key (kbd "DEL"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-S-z"))

(general-create-definer wmad/leader
  :states '(normal)
  :prefix "SPC")

(wmad/leader

  ;; [b] BUFFER ;;;;;;;;;;;;;;;;;;;;;;;
  "b"  '(:ignore t :which-key "Buffer")
  "bb" 'consult-buffer
  "b4" 'consult-buffer-other-window
  "b5" 'consult-buffer-other-frame

  ;; [c] CONSULT ;;;;;;;;;;;;;;;;;;;;;;;
  "c"  '(:ignore t :which-key "Consult")
  "cy" 'consult-yank-pop
  "cm" 'consult-mark
  "ci" 'consult-imenu
  "cI" 'consult-project-imenu
  "cf" 'consult-flycheck

  ;; [D] DEV DOCS ;;;;;;;;;;;;;;;;;;;;;;
  "D" '(:ignore t :which-key "Dev Docs")
  "Di" 'devdocs-browser-install-doc
  "Do" 'devdocs-browser-open
  "DO" 'devdocs-browser-open-in

  ;; [e] Evaluate ;;;;;;;;;;;;;;;;;;;
  "e"  '(:ignore t :which-key "Eval")
  "eb"  'eval-buffer
  "ee"  'eval-last-sexp

  ;; [E] Egot
  "E"  '(:ignore t :which-key "Eglot")
  "Er" 'eglot-rename
  "Ef" 'eglot-format
  "Er" 'eglot-find-references
  "Ed" 'eglot-find-definitions
  "E." 'completion-at-point

  ;; [h] HAIL HYDRA ;;;;;;;;;;;;;;;;;;
  "h"  '(:ignore t :which-key "Hydra")
  ;; see hydra.el

  ;; [g] GO ;;;;;;;;;;;;;;;;;;;;;;;
  "g"  '(:ignore t :which-key "Go")
  "gg" 'dumb-jump-go
  "gb" 'dumb-jump-back
  "gt"  '(:ignore t :which-key "Jump TO-DO")
  "gtn" 'hl-todo-next
  "gtp" 'hl-todo-previous
  "gto" 'hl-todo-occur
  "gti" 'hl-todo-insert

  ;; [n] Netsuite SDFCLI wrapper
  ;; temporary lib I am working on, name is likely to change.
  "n"  '(:ignore t :which-key "Netsuite")
  "nc" 'netsuite/create-project
  "nd" 'netsuite/deploy
  "nu" 'netsuite/upload-buffer
  "n1" 'netsuite/deploy21
  "n2" 'netsuite/upload-buffer21

  ;; [o] ORG-MODE ;;;;;;;;;;;;;;;;;;
  "o"  '(:ignore t :which-key "Org")
  "ob" 'org-brain-goto
  "oc" 'org-capture
  "oa" 'org-brain-agenda
  "oo"   'cfw:open-org-calendar

  ;; [p] PASSWORD-STORE ;;;;;;;;;;;;;;;;;
  "P"  '(:ignore t :which-key "Password")
  "Pc" 'password-store-copy
  "PC" 'password-store-clear
  "Pe" 'password-store-edit
  "Pi" 'password-store-insert
  "Pr" 'password-store-remove
  "PR" 'password-store-rename
  "Pg" 'password-store-generate
  "Pf" 'password-store-copy-field
  "Pu" 'password-store-url

  ;; [p] PROJECTILE ;;;;;;;;;
  "p" 'projectile-command-map

  ;; [s] SEARCH ;;;;;;;;;;;;;;;;;;;;;;
  "s" '(:ignore t :which-key "Search")
  "sR" 'consult-recent-file
  "sa" 'consult-apropos
  "sl" 'consult-line
  "sf" 'consult-find
  "sg" 'consult-grep
  "sr" 'consult-ripgrep
  "si" 'consult-isearch

  ;; XYZ ;;;;;;;;;;;;;;;;;;;;;;
  "<home>" 'wmad/open-dashboard
  "<end>"  'ibuffer
  "d"      'crux-duplicate-current-line-or-region
  "f"      'dired
  "m"      'magit-status
  "R"      'restart-emacs
  "W"      'olivetti-mode
  "x"      'hippie-expand
  "y"      'yas-describe-tables
  "z"      'origami-toggle-node
  ";"      'crux-duplicate-and-comment-current-line-or-region
  "!"      'evil-emacs-state

  ;; [w] WINDOW  ;;;;;;;;;;;;;;;;;;;;;;
  "w"  '(:ignore t :which-key "Window")
  ;; a more comprehensive collection is bound to C-w

  ;; FIXME better use hydra for this scenario.
  "w <left>"  'shrink-window-horizontally
  "w <right>" 'enlarge-window-horizontally
  "w <up>"    'shrink-window
  "w <down>"  'enlarge-window

  "<left>"    'windmove-left
  "<right>"   'windmove-right
  "<up>"      'windmove-up
  "<down>"    'windmove-down

  "w0"        'delete-window
  "w1"        'delete-other-windows
  "w2"        'split-window-below
  "w3"        'split-window-right
  "w5"        'delete-frame

  "wh"        'split-window-below
  "wv"        'split-window-right

  "w-"        'split-window-below
  "w\\"       'split-window-right
  "wt"        'crux-transpose-windows)


(require 'evil)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-global-set-key 'normal (kbd "C-z") 'undo)
(evil-global-set-key 'normal (kbd "C-S-z") 'undo-redo)
(evil-global-set-key 'normal (kbd "U") 'undo-redo)

(evil-global-set-key 'emacs (kbd "C-e") 'evil-normal-state)

(defvar my-consult-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-s" #'previous-history-element)
    map))

(require 'consult)
(consult-customize consult-line :keymap my-consult-line-map)

(require 'move-text)
(move-text-default-bindings)

;;; bindings.el ends here
