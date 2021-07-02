;;; misc.el --- Miscelanea related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(wmad/package-install 'restart-emacs)
(wmad/package-install 'hnreader)
(wmad/package-install 'writefreely)
(wmad/package-install 'crux)
(wmad/package-install 'helpful)
(wmad/package-install 'devdocs-browser)


(wmad/package-install 'emojify)
(require 'emojify)
(add-hook 'erc-mode-hook #'emojify-mode)


(wmad/package-install 'olivetti)
(require 'olivetti)
(setq olivetti-body-width 165)


(wmad/package-install 'elpher)
(require 'elpher)
(setq gnutls-verify-error 'nil)


(wmad/package-install 'emacs-everywhere)
(require 'emacs-everywhere)
;; slightly bigger window
(setq emacs-everywhere-frame-parameters
      '((name . "emacs-everywhere")
        (width . 80)
        (height . 40)))
;; force org-mode loading
(setq emacs-everywhere-init-hooks
      '(emacs-everywhere-set-frame-name
        emacs-everywhere-set-frame-position
        org-mode
        emacs-everywhere-insert-selection
        emacs-everywhere-remove-trailing-whitespace
        emacs-everywhere-init-spell-check))


(wmad/package-install 'which-key)
(require 'which-key)
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 10000)
(setq which-key-idle-secondary-delay 0.05)
(setq which-key-sort-order 'which-key-key-order-alpha)
(which-key-mode 1)

;;; misc.el ends here
