;; pkgsys.el --- Core configuration : Package System -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

(defun wmad-pkgsys-init ()
  "Initialize package system."
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")
               '("org" . "https://orgmode.org/elpa/"))

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-and-compile
    (setq use-package-always-ensure nil)
    (setq use-package-always-defer nil)
    (setq use-package-always-demand nil)
    (setq use-package-expand-minimally nil)
    (setq use-package-enable-imenu-support t)
    (setq use-package-compute-statistics nil)
    (setq use-package-hook-name-suffix nil))

  (eval-when-compile
    (require 'use-package)))

(provide 'wmad-pkgsys)

;;; wmad-pkgsys.el ends here
