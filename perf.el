;;; perf.el --- Performance related config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; https://gitlab.com/nathanfurnal/dotemacs/-/blob/master/init.el
;; Speed up startup High garbage collection at startup needs to be
;; reset at some point then we defer the work to `gcmh'.
(add-hook 'emacs-startup-hook
	        (lambda ()
	          (setq gc-cons-threshold 16777216 ; 16mb
		              gc-cons-percentage 0.1)))


(require 'warnings)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq warning-suppress-types '((comp)))



(if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t)
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
