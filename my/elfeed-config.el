;;; elfeed-conf.el --- Elfeed config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; nice article: https://noonker.github.io/posts/2020-04-22-elfeed/
;;; Code:

(require 'elfeed)
(setq elfeed-feeds
      '(("https://planet.emacslife.com/atom.xml" emacs)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" youtube emacs)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxkMDXQ5qzYOgXPRnOBrp1w" youtube emacs)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" youtube emacs)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMV8p6Lb-bd6UZtTc_QD4zA" youtube lisp)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7eKF0lPY8LNwfczq9UFlxg" youtube music)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCS-XQZQxiuC7UoRPqrXyqrg" youtube music)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCaisXKBdNOYqGr2qOXCLchQ" youtube music)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJh6mkP4xecfoosX95I1Otg" youtube music)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UClQT6Vnsm6BUm0I5kR26EkQ" youtube music)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCknVpWR6m2Ijzkqo-aPXs_g" youtube music)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkqjcQwTNvsxQWT44N6ESrA" youtube music)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCvXxdkt1d8Uu08NAQP2IUTw" youtube chess)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCQHX6ViZmPsWiYSFAyS0a3Q" youtube chess)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCmEClzCBDx-vrt0GuSKBd9g" youtube nerd)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqoAEDirJPjEUFcF2FklnBA" youtube nerd)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHugE6eRhqB9_AZQh4DDbIw" youtube news)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsTcErHg8oDvUnTzoqsYeNw" youtube news)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSHZKyawb77ixDdsGog4iWA" youtube podcast)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBJycsmduvYEL83R_U4JriQ" youtube geek)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A" youtube math)))

(require 'elfeed-dashboard)
(setq elfeed-dashboard-file (concat user-emacs-directory "my/elfeed-dashboard.org"))

;; Nice!
;; https://karthinks.com/software/declickbait-elfeed/
(add-hook 'elfeed-new-entry-hook #'elfeed-declickbait-entry)

(defun elfeed-declickbait-entry (entry)
  (let ((title (elfeed-entry-title entry)))
    (setf (elfeed-meta entry :title)
          (elfeed-title-transform title))))

(defun elfeed-title-transform (title)
  "Declickbait string TITLE."
  (let* ((trim "\\(?:\\(?:\\.\\.\\.\\|[!?]\\)+\\)")
         (arr (split-string title nil t trim))
         (s-table (copy-syntax-table)))
    (modify-syntax-entry ?\' "w" s-table)
    (with-syntax-table s-table
      (mapconcat (lambda (word)
                   (cond
                    ((member word '("AND" "OR" "IF" "ON" "IT" "TO"
                                    "A" "OF" "VS" "IN" "FOR" "WAS"
                                    "IS" "BE"))
                     (downcase word))
                    ((member word '("WE" "DAY" "HOW" "WHY" "NOW" "OLD"
                                    "NEW" "MY" "TOO" "GOT" "GET" "THE"
                                    "ONE" "DO" "YOU"))
                     (capitalize word))
                    ((> (length word) 3) (capitalize word))
                    (t word)))
                 arr " "))))


(defun wmad/open-url-at-point-in-mpv ()
  "Open URL at point in MPV."
  (interactive)
  (let ((command (format "mpv '%s'" (thing-at-point 'url))))
    ;; (async-shell-command command)))
    (start-process-shell-command command nil command)))

;;; elfeed-conf.el ends here
