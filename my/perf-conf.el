;;; perf-conf.el --- Performance config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Garbage collection stuff
;; https://gitlab.com/nathanfurnal/dotemacs/-/blob/master/init.el
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

;;; Code:

(setup (:package gcmh)
  (:hide-mode)
  (setq gcmh-mode 1)
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold (* 16 1024 1024)))

(if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t
          native-comp-deferred-compilation t
          native-comp-async-query-on-exit t
          native-comp-async-jobs-number 6
          native-comp-async-report-warnings-errors nil)
  (message "Native compilation is *not* available, consider enabling it."))

(unless (functionp 'json-serialize)
  (message "Native JSON is *not* available, consider enabling it."))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; perf-conf.el ends here
