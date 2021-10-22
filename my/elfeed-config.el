;;; elfeed-conf.el --- Elfeed config ;; -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; nice article: https://noonker.github.io/posts/2020-04-22-elfeed/

;;; Code:

(setup (:package elfeed)
  (require 'elfeed)
  (setq elfeed-feeds
        '(("https://planet.emacslife.com/atom.xml" emacs)
          ("https://phys.org/rss-feed/breaking/physics-news/" physics)
          ("https://phys.org/rss-feed/breaking/science-news/mathematics/" math)
          ("https://phys.org/rss-feed/breaking/space-news/" science space)
          ("https://phys.org/rss-feed/breaking/technology-news/computer-sciences/" compsci)
          ("https://phys.org/rss-feed/breaking/technology-news/consumer-gadgets/" gadgets)
          ("https://phys.org/rss-feed/breaking/technology-news/internet/" web)
          ("https://phys.org/rss-feed/breaking/technology-news/machine-learning-ai/" ai)
          ("https://phys.org/rss-feed/breaking/biology-news/biotechnology/" science biotech)
          ("https://phys.org/rss-feed/breaking/chemistry-news/biochemistry/" science biochemistry)
          ("https://feeds.arstechnica.com/arstechnica/technology-lab" tech)
          ("https://feeds.arstechnica.com/arstechnica/gadgets" gadgets)
          ("https://feeds.arstechnica.com/arstechnica/science" science)
          ("https://news.ycombinator.com/rss" misc)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" video emacs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxkMDXQ5qzYOgXPRnOBrp1w" video emacs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" video emacs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMV8p6Lb-bd6UZtTc_QD4zA" video lisp)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7eKF0lPY8LNwfczq9UFlxg" video music)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCS-XQZQxiuC7UoRPqrXyqrg" video music)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCaisXKBdNOYqGr2qOXCLchQ" video music)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJh6mkP4xecfoosX95I1Otg" video music)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UClQT6Vnsm6BUm0I5kR26EkQ" video music)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCknVpWR6m2Ijzkqo-aPXs_g" video music)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkqjcQwTNvsxQWT44N6ESrA" video music)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSJ4gkVC6NrvII8umztf0Ow" video music)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCvXxdkt1d8Uu08NAQP2IUTw" video chess)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCQHX6ViZmPsWiYSFAyS0a3Q" video chess)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCmEClzCBDx-vrt0GuSKBd9g" video nerd)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqoAEDirJPjEUFcF2FklnBA" video nerd)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHugE6eRhqB9_AZQh4DDbIw" video news)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsTcErHg8oDvUnTzoqsYeNw" video news)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSHZKyawb77ixDdsGog4iWA" video podcast)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBJycsmduvYEL83R_U4JriQ" video geek)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A" video math))))

(require 'elfeed)
(add-hook 'elfeed-new-entry-hook 'elfeed-declickbait-entry)
(setq elfeed-dashboard-file (concat user-emacs-directory "my/elfeed-dashboard.org"))

;; Nice!
;; https://karthinks.com/software/declickbait-elfeed/
;; (add-hook 'elfeed-new-entry-hook #'elfeed-declickbait-entry)

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
    (start-process-shell-command command nil command)))

;;; elfeed-conf.el ends here
