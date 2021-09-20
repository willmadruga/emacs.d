;;; init.el --- Emacs init ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; TODO: Build EMACS 28 with native-compilation:
;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation

;;; Code:

(setq package-archives
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("GNU ELPA" . 10)
        ("MELPA"    . 5)))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (require 'use-package)
  (setq use-package-hook-name-suffix nil))

;; TODO:
;; attempt something new as for my keybindings
;; My left index finger is hurting, I don't think I can continue with emacs keybindigs
;; on my keyboard, it's been awhile already and I'm starting to feel the pain on my joints :/
;; can't do evil mode either, I'm too undisciplined for it... if I'm not in VIM, i'da rather not use
;; alternatives are?
;; - that menu from magit, transient... I saw an article recently about that... maybe give it a try

;; FIXME: should I continue load-file or switch back to single init.el?
;; assess the init time and judge if load-file is slowing it down after moving back to use-package...
(dolist (fname
         '(
           "my/perf-conf.el"
           "my/conf.el"
           "my/ide-conf.el"
           "my/js-conf.el"
           "my/sdfcli.el"
           "my/org-conf.el"
           "my/ui-conf.el"
           "my/keyb-conf.el"
           "password-store.el"
           ;; "my/snitch-conf.el" ;; trying it out... facing src-fn issue, hangs exwm...
           "my/exwm-conf.el"
           ))
  (load-file (expand-file-name fname user-emacs-directory)))


;; set GC back to normal. Value increased in early-init
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
