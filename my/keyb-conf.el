;;; keyb-conf.el --- Key Bindings config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; https://github.com/jerrypnz/major-mode-hydra.el

;;; Code:

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-S-z"))

(move-text-default-bindings)

(require 'pretty-hydra)
;; FIXME: work on a better title. there is an example with favicon in the package home page.
(pretty-hydra-define wmad-global-keys (:foreign-keys warn :title "[ Global Keys ]" :quit-key "q")

  ("Consult"
   (("ca" consult-apropos     "apropos")
    ("cb" consult-buffer      "buffer")
    ("cc" consult-flycheck    "flycheck")
    ("cf" consult-find        "find")
    ("cg" consult-grep        "grep")
    ("cl" consult-line        "line")
    ("ci" consult-imenu       "imenu")
    ("cr" consult-ripgrep     "rg")
    ("cR" consult-recent-file "recent-file")
    ("cs" consult-isearch     "isearch"))

   "Dev"
   (("dd" crux-duplicate-current-line-or-region "duplicate")
    ("dc" crux-duplicate-and-comment-current-line-or-region "duplicate + comment")
    ("du" undo "undo")
    ("di" devdocs-browser-install-doc "devdocs install")
    ("dl" devdocs-browser-list-docs   "devdocs list")
    ("do" devdocs-browser-open        "devdoc open")
    ("mb" magit-blame "Magit Blame"))

   "Emacs"
   (("D" dired "Dired")
    ("WD" wdired-change-to-wdired-mode "Change to wdired")
    ("B" speedbar              "speedbar")
    ("I" ibuffer               "i-buffer")
    ("S" ansi-term             "ANSI Shell")
    ("y" yank-from-kill-ring   "Yank from Kill-Ring")
    ("Y" yas-describe-tables   "YASnippet Table")
    ("uu" popper-toggle-latest "Popper Toggle")
    ("uc" popper-cycle         "Popper Cycle")
    ("ut" popper-toggle-type   "Popper Toggle Type")
    ("C-SPC" nil               "Hydra Major Mode"))

   "ORG"
   (("ot" org-roam-buffer-toggle      "Org-Roam Toggle")
    ("of" org-roam-node-find          "Org-Roam Find")
    ("oi" org-roam-node-insert        "Org-Roam Insert")
    ("os" org-save-all-org-buffers    "Save ALL org buffers")
    ("oa" org-agenda                  "Org Agenda")
    ("oT" org-timer-set-timer         "Org Set Timer")
    ("oP" org-timer-pause-or-continue "Org Pause/Continue Timer"))

   "Password"
   (("pc" password-store-copy       "Copy Password")
    ("pf" password-store-copy-field "Copy Field"))

   "Tab-Bar"
   (("tn" tab-new)
    ("tr" tab-rename)
    ("tc" tab-close)
    ("tt" tab-recent)
    ("tl" tab-list)
    ("t SPC"     toggle-frame-tab-bar)
    ("t <right>" tab-next)
    ("t <left>"  tab-previous)
    ("t <up>"    tab-move))

   "Window"
   (("<up>"    enlarge-window)
    ("<down>"  shrink-window)
    ("<left>"  enlarge-window-horizontally)
    ("<right>" shrink-window-horizontally))

   "EXWM"
   (("s-RET" nil   "Alacritty")
    ("s-&" nil     "Execute command")
    ("s 1-9" nil   "Switch Workspaces 1 to 9")
    ("s arrow" nil "Window Move direction"))
   ))

(global-set-key (kbd "M-SPC") 'wmad-global-keys/body)

;; How can I integrate these to hydra?
(global-set-key (kbd "C-c o d") 'org-roam-dailies-map)
(define-key org-roam-dailies-map (kbd "Y") 'org-roam-dailies-capture-yesterday)
(define-key org-roam-dailies-map (kbd "T") 'org-roam-dailies-capture-tomorrow)



(require 'major-mode-hydra)
(global-set-key (kbd "C-SPC") #'major-mode-hydra)



(major-mode-hydra-define js2-mode nil
  ("Eglot"
   (("D" eglot-find-declaration "Find Declaration")
    ("i" eglot-find-implementation "Find Implementation")
    ("f" eglot-format "Format")
    ("r" eglot-rename "Rename"))

   "SuiteCloud"
   (("c" netsuite/create-project "Create SDF Project")
    ("d" netsuite/deploy "Deploy SDF")
    ("u" netsuite/upload-buffer "Upload buffer"))

   "Dumb jump"
   (("g" dumb-jump-go   "Go")
    ("b" dumb-jump-back "Back"))))



(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer       "buffer")
    ("d" eval-defun        "defun")
    ("e" eval-last-sexp    "s-expr")
    ("r" eval-region       "region")
    ("p" eval-print-last-sexp "print s-expr")) ;; pp-eval-last-sexp

   "REPL"
   (("I" ielm "ielm"))

   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))

   "Doc"
   (("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

;;; keyb-conf.el ends here
