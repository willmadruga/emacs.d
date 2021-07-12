;;; perf.el --- Performance related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Remember to build emacs 28 with native-compilation:
;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation

;;; Code:

(add-hook 'after-init-hook
	        (lambda ()
	          (setq gc-cons-threshold 800000)))

(require 'warnings)
(setq read-process-output-max (* 1024 1024))
(setq warning-suppress-types '((comp)))



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


;; https://gitlab.com/nathanfurnal/dotemacs/-/blob/master/init.el
;; Adopt a sneaky garbage collection strategy of waiting until idle
;; time to collect; staving off the collector while the user is
;; working.  Thanks Doom -
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
(wmad/package-install 'gcmh)
(require 'gcmh)
(setq gcmh-mode 1)
(setq gcmh-idle-delay 5)
(setq gcmh-high-cons-threshold (* 16 1024 1024))


;;; perf.el ends here
