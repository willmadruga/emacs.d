;;; rational-addon-term.el --- Terminal config       -*- lexical-binding: t; -*-

(rational-package-install-package 'vterm)
(rational-package-install-package 'shx)
(rational-package-install-package 'better-shell)
(rational-package-install-package 'xterm-color)

(setenv "PAGER" "cat")
(shx-global-mode 1)

;; much improved perf in shell-mode buffers
;; https://stackoverflow.com/questions/26985772/fast-emacs-shell-mode
(setq comint-move-point-for-output nil)
(setq comint-scroll-show-maximum-output nil)

(setq vterm-max-scrollback 10000)
(with-eval-after-load 'evil (advice-add 'evil-collection-vterm-insert
                                        :before #'vterm-reset-cursor-point))

(setq extended-path '("~/bin" "~/.local/bin" "~/.nvm/versions/node/v18.5.0/bin" ))

;; Help out MacOS users to make dev env more like-linux
(when (file-directory-p "/opt/homebrew")
  (setq gnubin-locations (split-string (shell-command-to-string "find /opt/homebrew -name \"*gnubin*\" -type d") "\n" t))
  (add-to-list 'gnubin-locations "/opt/homebrew/bin" t)
  (add-to-list 'gnubin-locations "/opt/homebrew/sbin" t)
  (dolist (item gnubin-locations)
    (add-to-list 'extended-path item)))

(dolist (p extended-path)
  (add-to-list 'exec-path p))

(setenv "PATH" (string-trim-right (string-join exec-path ":") ":$"))

(provide 'rational-addon-term)
