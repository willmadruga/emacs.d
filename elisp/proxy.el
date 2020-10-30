;; From: https://github.com/syl20bnr/spacemacs/issues/4807

;; There is a bug in url-http with the function url-https-proxy-connect: It will not send a
;; Proxy-Authorization header, so all https calls through an authenticating proxy will fail.
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=42422, fixed in Emacs 28.1 apparently.

;; With orgmode.org/elpa and melpa changing to https only, this problem became more visible.

;; As a workaround till 28.1 arrives, override the function with fixed version:
;; (with-eval-after-load 'url-http
;;   (defun url-https-proxy-connect (connection)
;;     (setq url-http-after-change-function 'url-https-proxy-after-change-function)
;;     (process-send-string connection (format (concat "CONNECT %s:%d HTTP/1.1\r\n"
;; 						    "Host: %s\r\n"
;; 						    (let ((proxy-auth (let ((url-basic-auth-storage
;; 									     'url-http-proxy-basic-auth-storage))
;; 									(url-get-authentication url-http-proxy nil 'any nil))))
;; 						      (if proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))
;; 						    "\r\n")
;; 					    (url-host url-current-object)
;; 					    (or (url-port url-current-object)
;; 						url-https-default-port)
;; 					    (url-host url-current-object)))))

;; Proxy settings:
;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;      ("http" . "proxy:80")
;;      ("https" . "proxy:80")))
