;; wmad-fun.el --- Fun Configuration Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun wmad/fun-init ()
  "Fun stuff configuration."

  (use-package form-feed
    ;; https://depp.brause.cc/form-feed/
    :config
    (global-form-feed-mode))

  ;; Reading epub format :)
  ;; Visit https://depp.brause.cc/nov.el/ for configuration...
  (use-package nov
    :config
    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

  ;; Game of Life...
  (use-package xbm-life)

  ;; Bookmark importer
  (load "~/src/bookmarks/bm-import.el")

  )

(provide 'wmad-fun)
;;; wmad-fun.el ends here
