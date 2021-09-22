;;; keyb-conf.el --- Key Bindings config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; https://github.com/jerrypnz/major-mode-hydra.el
;;
;; Using major-mode-hydra could be a transition step into getting start with transient.
;; Had to change from regular Emacs keybindings ASAP because my fingers were hurting and it can take awhile
;; before I have a working environment with a transient setup.
;;
;; Timely, these articles on reddit will help out expediting this task:
;; https://www.reddit.com/r/emacs/comments/m518xh/transient_api_example_alternative_bindings_part_1/
;; https://www.reddit.com/r/emacs/comments/pon0ee/transient_api_example_part_2_transientdostay/
;;

;;; Code:

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-S-z"))

(move-text-default-bindings)

(require 'pretty-hydra)
(require 'yasnippet)
(require 'popper)
(require 'org-roam)
(require 'org-roam-dailies)
(require 'eglot)
(require 'js2-mode)

(require 'password-store "../../password-store.el")
(require 'sdfcli "../../my/sdfcli.el")

(pretty-hydra-define wmad-consult-keys (:foreign-keys warn :title "[ Consult Keybindings ]" :exit t)
  ("Consult"
   (("a"   consult-apropos     "apropos")
    ("b"   consult-buffer      "buffer")
    ("f"   consult-flycheck    "flycheck")
    ("F"   consult-find        "find")
    ("g"   consult-grep        "grep")
    ("l"   consult-line        "line")
    ("i"   consult-imenu       "imenu")
    ("r"   consult-ripgrep     "rg")
    ("R"   consult-recent-file "recent-file")
    ("s"   consult-isearch     "isearch"))
   "Quit"
   (("q"   hydra-keyboard-quit)
    ("C-g" hydra-keyboard-quit))))

(pretty-hydra-define wmad-dev-keys (:foreign-keys warn :title "[ Dev Keybindings ]" :exit t)
  ("Dev"
   (("c"   crux-duplicate-and-comment-current-line-or-region "duplicate + comment")
    ("d"   crux-duplicate-current-line-or-region             "duplicate")
    ("u"   undo                                              "undo")
    ("i"   devdocs-browser-install-doc                       "devdocs install")
    ("l"   devdocs-browser-list-docs                         "devdocs list")
    ("o"   devdocs-browser-open                              "devdoc open")
    ("b"   magit-blame                                       "Magit Blame"))
   "Quit"
   (("q"   hydra-keyboard-quit)
    ("C-g" hydra-keyboard-quit))))

(pretty-hydra-define wmad-general-keys (:foreign-keys warn :title "[ General Keybindings ]" :exit t)
  ("Emacs"
   (("b"   speedbar                     "Speedbar")
    ("d"   dired                        "Dired")
    ("i"   ibuffer                      "I-Buffer")
    ("pp"  popper-toggle-latest         "Popper Toggle")
    ("pc"  popper-cycle                 "Popper Cycle")
    ("pt"  popper-toggle-type           "Popper Toggle Type")
    ("s"   ansi-term                    "ANSI Shell")
    ("w"   wdired-change-to-wdired-mode "Dired Edit mode")
    ("y"   yank-from-kill-ring          "Yank from Kill-Ring")
    ("Y"   yas-describe-tables          "YASnippet Table"))
   "Quit"
   (("q"   hydra-keyboard-quit)
    ("C-g" hydra-keyboard-quit))))

(pretty-hydra-define wmad-org-keys (:foreign-keys warn :title "[ ORG Keybindings ]" :exit t)
  ("ORG"
   (;;("rd"  #'org-roam-dailies-map      "Org-Roam Dailies") ;; how do I?
    ("rt"  org-roam-buffer-toggle      "Org-Roam Toggle")
    ("rf"  org-roam-node-find          "Org-Roam Find")
    ("ri"  org-roam-node-insert        "Org-Roam Insert")
    ("s"   org-save-all-org-buffers    "Save ALL org buffers")
    ("a"   org-agenda                  "Org Agenda")
    ("tt"  org-timer-set-timer         "Org Set Timer")
    ("tp"  org-timer-pause-or-continue "Org Pause/Continue Timer"))
   "Quit"
   (("q"   hydra-keyboard-quit)
    ("C-g" hydra-keyboard-quit))))

(pretty-hydra-define wmad-tab-keys (:foreign-keys warn :title "[ Tab-Bar Keybindings ]" :exit t)
  ("Tab-Bar"
   (("n"         tab-new)
    ("r"         tab-rename)
    ("c"         tab-close)
    ("t"         tab-recent)
    ("l"         tab-list)
    ("SPC"     toggle-frame-tab-bar )
    ("<right>" tab-next)
    ("<left>"  tab-previous)
    ("<up>"    tab-move))
   "Quit"
   (("q"   hydra-keyboard-quit)
    ("C-g" hydra-keyboard-quit))))

(pretty-hydra-define wmad-global-keys (:foreign-keys warn :title "[ Keybindings ]" :exit t)
  (
   ""
   (("c" wmad-consult-keys/body "Consult")
    ("d" wmad-dev-keys/body "Development")
    ("e" wmad-general-keys/body "General")
    ("o" wmad-org-keys/body "ORG")
    ("t" wmad-tab-keys/body "Tab-bar"))

   ""
   (("pc" password-store-copy       "Copy Password")
    ("pf" password-store-copy-field "Copy Field"))

   ""
   (("<up>"    enlarge-window)
    ("<down>"  shrink-window)
    ("<left>"  enlarge-window-horizontally)
    ("<right>" shrink-window-horizontally))

   ""
   (("s-RET"   nil "Alacritty")
    ("s-&"     nil "Execute command")
    ("s 1-9"   nil "Switch Workspaces 1 to 9")
    ("s arrow" nil "Window Move direction"))

   ""
   (("C-SPC" #'major-mode-hydra))

   "Quit"
   (("q"   hydra-keyboard-quit)
    ("C-g" hydra-keyboard-quit))))


(global-set-key (kbd "M-SPC") 'wmad-global-keys/body)
(global-set-key (kbd "M-RET") 'wmad-global-keys/body)

(global-set-key (kbd "C-c o d") 'org-roam-dailies-map)

(require 'major-mode-hydra)
(global-set-key (kbd "C-SPC") #'major-mode-hydra)

(major-mode-hydra-define js2-mode nil
  ("Eglot"
   (("D" eglot-find-declaration)
    ("i" eglot-find-implementation)
    ("f" eglot-format)
    ("r" eglot-rename))

   "SuiteCloud"
   (("c" netsuite/create-project)
    ("d" netsuite/deploy)
    ("u" netsuite/upload-buffer)
    ("p" popper-toggle-latest))

   "Code Nav"
   (("g" dumb-jump-go)
    ("b" dumb-jump-back)
    ("m" imenu)
    ("h" js2-mode-hide-element)
    ("s" js2-mode-show-element))

   "Quit"
   (("q"   hydra-keyboard-quit)
    ("C-g" hydra-keyboard-quit))))


(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer)
    ("d" eval-defun)
    ("e" eval-last-sexp)
    ("r" eval-region)
    ("p" eval-print-last-sexp)) ;; pp-eval-last-sexp

   "REPL"
   (("I" ielm))

   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))

   "Quit"
   (("q"   hydra-keyboard-quit)
    ("C-g" hydra-keyboard-quit))))


;;; keyb-conf.el ends here
