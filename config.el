;;; config.el --- Rational Config                                   -*- lexical-binding: t; -*-

;; Commentary

;;; Code:

(require 'rational-defaults)
(require 'rational-evil)
(require 'rational-ui)
(require 'rational-completion)
(require 'rational-project)
(require 'rational-editing)
(require 'rational-org)
(require 'rational-use-package)
(require 'rational-windows)
(require 'rational-startup)
(require 'rational-updates)
(require 'rational-addon-javascript)
(require 'rational-addon-suitecloud)

;;; OSX ;;;;;;;;;;
(if (eq system-type 'darwin)
(require 'rational-osx))

;;; PACKAGE INSTALL ;;;;;;;;;;
(rational-package-install-package 'doom-themes)
(rational-package-install-package 'popper)
(rational-package-install-package 'diminish)
(rational-package-install-package 'elisp-format)
(rational-package-install-package 'magit)
(rational-package-install-package 'aggressive-indent)
(rational-package-install-package 'yasnippet)
(rational-package-install-package 'dired-single)

;;; MISC ;;;;;;;;;;
(column-number-mode 1)

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq tab-always-inden 'complete)
(setq tab-width 2)
(setq fill-column 140)

(setq read-symbol-positions-list nil) ;; fix helpful issue

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c v") 'vterm)
(global-set-key (kbd "C-c H") 'windmove-left)
(global-set-key (kbd "C-c J") 'windmove-down)
(global-set-key (kbd "C-c K") 'windmove-up)
(global-set-key (kbd "C-c L") 'windmove-right)

;;; UI ;;;;;;;;;;
(custom-set-variables '(rational-ui-default-font '(:font "FiraCode Nerd Font"
:weight light
:height 185)))

(load-theme 'doom-gruvbox t)
(doom-modeline-mode 1)

;;; POPPER ;;;;;;;;;;
(setq popper-reference-buffers '("\\*Messages\\*" "Output\\*$" "\\*Async Shell Command\\*" "\\*Flycheck errors\\*" "\\*Flymake diagnostics\\*" help-mode compilation-mode))
(setq popper-reference-buffers (append popper-reference-buffers '("^\\*eshell.*\\*$" eshell-mode "^\\*shell.*\\*$"  shell-mode "^\\*term.*\\*$"   term-mode "^\\*vterm.*\\*$"  vterm-mode)))
(global-set-key (kbd "C-c `") 'popper-toggle-latest)
(global-set-key (kbd "C-c t") 'popper-toggle-type) ;TODO: change keybinding
(global-set-key (kbd "C-c x") 'popper-cycle) ;TODO: change keybinding
(popper-mode +1)

;;; EVIL MODE ;;;;;;;;;;
(custom-set-variables '(evil-want-C-u-scroll t))
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-u") 'universal-argument)

;;; COMPLETIONS AND ACTIONS ;;;;;;;;;;
(define-key vertico-map (kbd "C-f") 'vertico-exit)
(define-key minibuffer-local-map (kbd "C-d") 'embark-act)
(define-key project-prefix-map (kbd "g") 'consult-ripgrep)
(global-set-key (kbd "C-M-j") 'consult-buffer)

;;; IDE ;;;;;;;;;;
(require 'magit)
(defalias 'git 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
(global-set-key (kbd "M-m") #'magit-status)

(aggressive-indent-mode)
(yas-global-mode 1)

(global-set-key (kbd "C-c f") 'elisp-format-buffer)

;;; ORG ;;;;;;;;;;;
(variable-pitch-mode 1) ;; Turn on variable pitch for non-monospace fonts

;;; ADDONS ;;;;;;;;;;
(setq rational-addons '("rational-addon-javascript" "rational-addon-suitecloud" "rational-addon-term"))
(dolist (file rational-addons)
(load (expand-file-name file rational-config-path) nil 'nomessage))

;;; OTHERS ;;;;;;;;;;

(define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
(define-key dired-mode-map [remap dired-up-directory]                 'dired-single-up-directory)
(define-key dired-mode-map [remap dired-find-file]                    'dired-single-buffer)
(customize-set-variable 'rational-startup-inhibit-splash t)
(diminish 'eldoc-mode)
(diminish 'yas-minor-mode)
(diminish 'evil-collection-unimpaired-mode)
(diminish 'whitespace-mode)

;;; SERVER ;;;;;;;;;;
;; (add-hook 'after-init-hook #'(lambda ()
;;                                (interactive)
;;                                (require 'server)
;;                                (or (server-running-p)
;;                                    (server-start))))
;;; config.el ends here
