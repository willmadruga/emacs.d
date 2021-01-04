;; wmad-devel-clj.el --- Development Configuration Layer : Clojure -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/devel-clj-init ()
  "Clojure configuration."
  
  ;; cider
  (use-package cider)
    ;; :init 
    ;;         (setq cider-stacktrace-default-filters '(tooling dup)
    ;;               cider-repl-pop-to-buffer-on-connect nil
    ;;               cider-prompt-save-file-on-load nil
    ;;               cider-repl-use-clojure-font-lock t)
    ;;         (add-hook 'clojure-mode-hook 'cider-mode)
    ;; :config
    ;;           (setq cider-prompt-for-symbol nil)
    ;;           (clojure-mode 1)
    ;;           (clojurec-mode 1)
    ;;           (clojurescript-mode 1)
    ;;           (cider-repl-mode 1)
    ;;           (clj-refactor-mode 1)
    ;;           (yas-minor-mode 1)
    ;;           )


  ;; cider eval sexp fu ;;;;;;;;;;;;;;;;;;;;
  (use-package cider-eval-sexp-fu)

  ;; clojure mode ;;;;;;;;;;;;;;;;;;;;
  (use-package clojure-mode)

  ;; clj-refactor ;;;;;;;;;;;;;;;;;;;;
  (use-package clj-refactor)

  ;; clojure snippets ;;;;;;;;;;;;;;;;;;;;
  (use-package clojure-snippets)

;; (defun my-clojure-mode-hook ()
;;     (clj-refactor-mode 1)
;;     (yas-minor-mode 1))

;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

  )

(provide 'wmad-devel-clj)
;;; wmad-devel-clj.el ends here
