;;; perf.el --- Performance related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      warning-suppress-types '((comp)))

(if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t)
  (message "Native compilation is *not* available, consider enabling it."))

(unless (functionp 'json-serialize)
  (message "Native JSON is *not* available, consider enabling it."))

;;; perf.el ends here
