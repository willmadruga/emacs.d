;;; snitch-conf.el --- Snitch config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; https://github.com/mrmekon/snitch-el/

;;; Code:

;; ==== COMMON CONFIG: DENY ====

;; A useful configuration is to deny all external communication by
;; default, but allow certain packages to communicate.  This example
;; demonstrates permitting only the ’elfeed’ package to create network
;; connections:

(require 'snitch) ;; src-fn is void....
(setq snitch-network-policy 'deny)
(setq snitch-process-policy 'deny)
(setq snitch-log-policy '(blocked whitelisted allowed))
(add-to-list 'snitch-network-whitelist
             (cons #'snitch-filter-src-pkg
                   '(
                     elfeed
                     )))
(snitch-mode +1)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; snitch-conf.el ends here
