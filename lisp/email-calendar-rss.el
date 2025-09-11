(use-package mu4e
  :ensure nil
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-refile-folder "/Archive"
        mu4e-trash-folder  "/Trash")
  (setq mu4e-maildir-shortcuts
        '((:maildir "/Archive" :key ?a)
          (:maildir "/inbox"   :key ?i)
          (:maildir "/work"    :key ?w)
          (:maildir "/sent"    :key ?s)))
  (setq mu4e-headers-fields
        '((:date          .  25)
          (:flags         .   6)
          (:from          .  22)
          (:subject       .  nil)))
  (add-to-list 'mu4e-bookmarks
               '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t))
  (setq mu4e-get-mail-command "mbsync fastmailchannel")
  (setq mu4e-compose-reply-to-address "matt@matthewlemon.com"
        user-mail-address "matt@matthewlemon.com"
        user-full-name  "Matthew Lemon")
  (setq message-signature "M R Lemon\n")
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-user "mrlemon@fastmail.com"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        smtpmail-smtp-server "smtp.fastmail.com")
  (setq message-kill-buffer-on-exit t))

(require 'mu4e-transient)
(global-set-key (kbd "C-c m") #'mu4e-transient-menu)

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '(("https://joeyh.name/blog/index.rss" debian linux)
          ("https://lukesmith.xyz/rss.xml" linux)
          ("https://www.tumfatig.net/index.xml" bsd)
          ("https://discoverbsd.com/feeds/posts/default" bsd)
          ("https://planet.debian.org/rss20.xml" debian)
          ("https://blog.cleancoder.com/atom.xml" programming)
          ("https://clojure.org/feed.xml" programming clojure)
          ("https://thelibre.news/latest/rss" freesoftware)
          ("https://drewdevault.com/blog/index.xml" freesoftware linux)
          ("https://landchad.net/rss.xml" linux)
          ("https://lobste.rs/rss" firehose)
          ("https://feeds.bbci.co.uk/news/rss.xml" news)
          ("https://www.coryzue.com/feed.xml" django programming)
          ("https://irreal.org/blog/?feed=rss2" emacs)
          ("https://www.metoffice.gov.uk/public/data/PWSCache/WarningsRSS/Region/dg" weather)
          ("https://baty.net/index.xml" personal productivity emacs)
          ("https://dataswamp.org/~solene/rss.xml" BSD)
          ("https://rubenerd.com/feed/" personal)
          ("https://simonwillison.net/atom/everything/" python AI programming)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCajqxDsE7PBMI_IkgMkQ39w" family)
          ("https://jvns.ca/atom.xml" linux)
          ("https://sive.rs/en.atom" discourse))))

(use-package org-caldav
  :config
  (setq org-caldav-url "http://radicale.banded-neon.ts.net/radicale/lemon")
  (setq org-icalendar-timezone "Europe/London")
  (setq org-caldav-sync-direction 'cal->org)
  (setq org-caldav-calendars
        '((:calendar-id "7c38e0c7-4a42-9863-c9e0-6025a32c4a65"
                        :files ("~/Documents/org/radcal.org")
                        :inbox "~/Documents/org/radbox.org")
          (:calendar-id "ae785050-e1f8-5d83-faa0-38eb10b6b53a"
                        :files ("~/Documents/org/radcal_coding.org")
                        :inbox "~/Documents/org/radbox_coding.org")
          (:calendar-id "e951175b-f02f-a759-5d25-3ca5d2a3d268"
                        :files ("~/Documents/org/radcal_work.org")
                        :inbox "~/Documents/org/radbox_work.org")
          (:calendar-id "bb48f855-f7bc-183f-f79d-275327d426d5"
                        :files ("~/Documents/org/radcal_alt.org")
                        :inbox "~/Documents/org/radbox_alt.org"))))

(provide 'email-calendar-rss)
