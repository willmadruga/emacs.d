;; wmad-keys-config.el --- Keybindings Configuration Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/keys-config ()
  "Keybindings configuration."
  (use-package emacs
    :config
    ;; Unsetting key bindings
    (global-unset-key (kbd "C-SPC"))
    (global-unset-key (kbd "C-z"))
    (global-unset-key (kbd "C-x C-z"))
    (global-unset-key (kbd "C-h h"))

    ;; Bring back the set mark command
    (global-set-key (kbd "C-SPC RET") 'set-mark-command)

    ;; Undo and Redo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (global-set-key (kbd "C-z")   'undo-fu-only-undo)
    (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

    ;; Window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (global-set-key (kbd "s-<left>")  'windmove-left)
    (global-set-key (kbd "s-<right>") 'windmove-right)
    (global-set-key (kbd "s-<up>")    'windmove-up)
    (global-set-key (kbd "s-<down>")  'windmove-down)
    (global-set-key (kbd "s-`")  'toggle-doom-alike-terminal)

    ;; Helpful help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (global-set-key (kbd "C-h f")   #'helpful-callable)
    (global-set-key (kbd "C-h v")   #'helpful-variable)
    (global-set-key (kbd "C-h k")   #'helpful-key)
    (global-set-key (kbd "C-c C-d") #'helpful-at-point)
    (global-set-key (kbd "C-h F")   #'helpful-function)
    (global-set-key (kbd "C-h C")   #'helpful-command))


  ;; General Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package general
    :config
    (general-create-definer wmad/leader-keys
      :prefix "C-SPC"
      :global-prefix "C-SPC")

    ;; Chords definitions
    (general-define-key
     "C-c <down>" 'wmad/duplicate-line
     "C-c d" 'delete-trailing-whitespace)


    ;; General Set of keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "e"   '(wmad/open-init-file :which-key "Open init file")
      "j"   '(wmad/open-journal :which-key "Open journal file")
      "T"   '(wmad/open-todo :which-key "Open todo file")
      "k"   '(kill-buffer :which-key "Kill buffer")
      "f"   '(find-file :which-key "Find File")
      "SPC" '(projectile-find-file :which-key "Project Find File")
      "R"   '(restart-emacs :which-key "Restart Emacs")
      "v"   '(vterm :which-key "Terminal"))

    ;; Dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "d"     '(:ignore t :which-key "Dired")
      "dd"    '(dired-hide-dotfiles-mode :which-key "Hide dotfiles")
      "dj"    '(dired-jump :which-key "Jump")
      "ds"    '(dired-sidebar-show-sidebar :which-key "Show sidebar")
      "dh"    '(dired-sidebar-hide-sidebar :which-key "Hide sidebar")
      "d RET" '(dired-single-buffer :which-key "Single buffer"))

    ;; Org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "o"  '(:ignore t :which-key "Org-Mode")
      "oa" '(org-agenda :which-key "Agenda")
      "oc" '(org-capture :which-key "Capture")
      "ol" '(org-insert-link :which-key "Insert Link")
      "oo" '(org-open-at-point :which-key "Open Link"))

    ;; Project ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "p"  '(:ignore t :which-key "Project")
      "pc" '(projectile-command-map :which-key "All commands")
      "pf" '(projectile-find-file :which-key "Find File")
      "pp" '(projectile-switch-project :which-key "Switch Project")
      "pk" '(projectile-kill-buffers :which-key "Kill Buffers")
      "ps" '(projectile-ag :which-key "Silver Search")
      "pS" '(projectile-ripgrep :which-key "Ripgrep Search"))

    ;; Git ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "G"  '(:ignore t :which-key "Magit")
      "Gg" '(magit-dispatch :which-key "Git Menu")
      "Gf" '(magit-file-dispatch :which-key "Git File Menu")
      "Gs" '(magit-status :which-key "Status")
      "Gb" '(magit-blame :which-key "Blame")
      "GL" '(magit-log :which-key "Repo Log")
      "Gl" '(magit-log-buffer-file :which-key "Buffer file Log")
      "Gt" '(magit-todos-list :which-key "TODO List")
      "GF" '(magit-fetch :which-key "Fetch")
      "G <down>" '(magit-pull :which-key "Pull")
      "G <up>" '(magit-push :which-key "Push"))

    ;; Toggle things on/off ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "t"  '(:ignore t :which-key "Toggle")
      "td" '(dired-sidebar-toggle-sidebar :which-key "dired sidebar")
      "tf" '(toggle-frame-fullscreen :which-key "fullscreen")
      "th" '(load-theme :which-key "choose theme")
      "tm" '(menu-bar-mode :which-key "menu bar")
      "to" '(global-origami-mode :which-key "origami")
      "tt" '(bufler-tabs-mode :which-key "bufler tab bar"))

    ;; Netsuite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "n"  '(:ignore t :which-key "Netsuite")
      "nc" '(netsuite/create-project :which-key "Create Project")
      "nd" '(netsuite/deploy :which-key "Deploy Project")
      "nu" '(netsuite/upload-buffer :which-key "Upload buffer"))

    ;; Window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "w"  '(:ignore t :which-key "Window")
      "wt" '(wmad/transpose-windows :which-key "Transpose")
      "w-" '(split-window-below :which-key "Split below")
      "w=" '(split-window-right :which-key "Split right")
      "w0" '(delete-window :which-key "Delete this")
      "w1" '(delete-other-windows :which-key "Delete others")
      "w5" '(delete-frame :which-key "Delete frame")
      "w_" '(balance-windows :which-key "Balance")
      "wq" '(window-toggle-side-windows :which-key "Toggle Side windows"))

    ;; Buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "b"         '(:ignore t :which-key "Buffer")
      "bb"        '(bufler :which-key "Buffer Window")
      "bs"        '(bufler-switch-buffer :which-key "Switch Buffer")
      "b <right>" '(next-buffer :which-key "Next")
      "b <left>"  '(previous-buffer :which-key "Previous"))

    ;; Origami folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "z"  '(:ignore t :which-key "Origami")
      "za" '(origami-toggle-node :which-key "Toggle node")
      "zo" '(origami-open-node :which-key "Open")
      "zc" '(origami-close-node :which-key "Close"))

    ;; Go to... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "g"  '(:ignore t :which-key "Go to...")
      "gd" '(dumb-jump-go :which-key "definition")
      "gD" '(xref-find-definitions :which-key "definition")
      "gb" '(xref-pop-marker-stack :which-key "back")
      "gl" '(goto-line :which-key "line"))

    (wmad/leader-keys
     "l"  '(:ignore t :which-key "LSP")
     "ll" '(lsp :which-key "lsp mode")
     "lm" '(lsp-mode-map :which-keys "LSP mode map")
     "ld" '(dumb-jump-go :which-key "Dumb go")
     "lD" '(xref-find-definitions :which-key "Definitions")
     "lr" '(xref-find-references :which-keys "References")
     "ln" '(lsp-ui-find-next-reference :which-keys "Next")
     "lp" '(lsp-ui-find-prev-reference :which-keys "Previous")
     "le" '(lsp-ui-flycheck-list :which-keys "List")
     "ls" '(lsp-ui-sideline-mode :which-keys "sideline mode")
     "lx" '(lsp-execute-code-action :which-keys "Execute code action"))

    ;; Cider ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (wmad/leader-keys
      "c"  '(:ignore t :which-key "Cider")
      "cj" '(cider-jack-in :which-key "jack-in")
      "cb" '(cider-eval-buffer :which-key "eval buffer")
      "ce" '(cider-eval-last-sexp :which-key "eval sexp")
      "cc" '(cider-eval-print-last-sexp :which-key "eval sexp print")
      "cq" '(cider-quit :which-key "quit cider"))

  ))

(provide 'wmad-keys-config)
;;; wmad-keys-config.el ends here
