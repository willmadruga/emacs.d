;; wmad-devel-clj.el --- Development Configuration Layer : Clojure

;;; Commentary:

;;; Code:

(defun wmad/devel-clj-init ()

  ;; cider
  (use-package cider
    :ensure t)
    ;; :defer t
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
  (use-package cider-eval-sexp-fu :ensure t)

  ;; clojure mode ;;;;;;;;;;;;;;;;;;;;
  (use-package clojure-mode :ensure t)

  ;; clj-refactor ;;;;;;;;;;;;;;;;;;;;
  (use-package clj-refactor :ensure t)

  ;; clojure snippets ;;;;;;;;;;;;;;;;;;;;
  (use-package clojure-snippets :ensure t)

;; (defun my-clojure-mode-hook ()
;;     (clj-refactor-mode 1)
;;     (yas-minor-mode 1))

;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

  )

(provide 'wmad-devel-clj)
;;; wmad-devel-clj.el ends here
