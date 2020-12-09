;; wmad-core.el --- Core configuration : Entry-point

;;; Commentary:

;;; Code:

(require 'wmad-pkgsys)
(require 'wmad-emacs-config)
(require 'wmad-themes-config)
(require 'wmad-keys-config)
(require 'wmad-dired-config)

(require 'wmad-fns)
(require 'wmad-org)

(require 'wmad-base-pkgs)
(require 'wmad-devel-pkgs)

(require 'wmad-devel-lsp)
(require 'wmad-devel-js)
(require 'wmad-devel-clj)
(require 'wmad-devel-el)
(require 'wmad-devel-lisp)


(defun wmad/init ()
  "Emacs configuration initialization."
  (wmad/load-private-scripts)

  (wmad/emacs-config)
  (wmad-pkgsys-init)
  (wmad/keys-config)
  (wmad/theme-config)
  (wmad/org-config)
  (wmad/dired-config)
  
  (wmad/base-packages-init)
  (wmad/devel-packages-init)
  (wmad/devel-lsp-init)
  (wmad/devel-js-init)
  (wmad/devel-clj-init)
  (wmad/devel-elisp-init)
  (wmad/devel-lisp-init)
  )

(provide 'wmad-core)
;;; wmad-core.el ends here
