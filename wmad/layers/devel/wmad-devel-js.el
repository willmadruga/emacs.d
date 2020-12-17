;; wmad-devel-js.el --- Development Configuration Layer : Javascript -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-js-init ()
  "Javascript configuration."

  ;; js mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package js2-mode
    :ensure t
    :config
    (setq js-indent-level 2)
    
    ;; Don't use built-in syntax checking
    (setq js2-mode-show-strict-warnings nil)
    
    )

  ;; js refactor ;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package js2-refactor
    :ensure t
    :hook (js2-mode-hook . js2-refactor-mode)
    ;; :config (add-hook 'js2-mode-hook
    ;;                   #'js2-refactor-mode))
    )

  ;; "Netsuite-Mode" package: name is yet to be defined.
  ;; that's my package, basically a wrapper around SDFCLI
  (use-package emacs
    :config
    (load-file "~/src/netsuite-mode.el/netsuite.el")
    (add-to-list 'auto-mode-alist
                 '("\\.js\\'" . netsuite-mode)
                 '("\\.js\\'" . display-fill-column-indicator-mode))
    (setq fill-column 140))

  )

(provide 'wmad-devel-js)
;;; wmad-devel-js.el ends here
