;; Some combination of GNU TLS and Emacs fail to retrieve archive
;; contents over https.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; this is also included in the configuration.org file
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
(package-initialize)

;;(load-config)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; always ensure package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; auto package update
(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; remove certain minor modes from the mode line
(use-package diminish)

;; use new GNU Elpa Key update
(use-package gnu-elpa-keyring-update)

;; ID information
(setq user-full-name "Matthew Lemon")
(setq user-mail-address "matt@matthewlemon.com")

;; Core bindings
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Use iBuffer instead of Buffer List
(global-set-key (kbd "C-x C-b") #'ibuffer)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

;; ox-hugo
(use-package ox-hugo
  :after ox)

;; Deal with history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode +1)
(setq savehist-save-minibuffer-history +1)
(setq savehist-additional-vriables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; popup kill-ring
(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

;; Disover my major
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

;; Some basics
(setq auto-save-default nil)
(global-set-key (kbd "M-o") 'other-window)
(setq column-number-mode t)
(setq apropos-do-all t)
(setq x-stretch-cursor t)
;; Emacs use own password prompt, not external PIN entry program
(setenv "GPG_AGENT_INFO" nil)
(setq kmacro-ring-max 30)

;; encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; binds
(bind-keys ("M-1" . delete-other-windows)
           ("M-O" . mode-line-other-buffer))

;; hydra
(use-package hydra
  :config
  (setq hydra-lv nil))

(defhydra hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("_" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

(bind-keys ("C-x C-0" . hydra-zoom/body)
           ("C-x C-=" . hydra-zoom/body)
           ("C-x C--" . hydra-zoom/body)
           ("C-x C-+" . hydra-zoom/body))

;; calendar proper Monday start
(setq calendar-week-start-day 1)
(setq calendar-date-style (quote european))

;; beacon mode
(use-package beacon
  :config
  (setq beacon-color "OrangeRed")
  (beacon-mode 1))

;; kill other buffers
(defun kill-other-buffers ()
   "Kill all other buffers."
   (interactive)
   (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; mu4e
;; the exact path may differ --- check it
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; deal with horrible grey backgrounds in HTML backgrounds
(require 'mu4e-contrib)
(require 'org-mu4e)
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 60)
(setq shr-color-visible-distance-min 5)
(setq shr-use-colors nil)
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))


;; email settings
(setq mu4e-maildir "~/.mail/matt-matthewlemon.com")
(setq message-send-mail-function 'smtpmail-send-it)
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Fastmail"
           :enter-func (lambda () (mu4e-message "Entering Fastmail context"))
           :leave-func (lambda () (mu4e-message "Leaving Fastmail context"))
           ;; we match based on the contact-fields of the message
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                              :to "matt@matthewlemon.com")))
           :vars '((mu4e-sent-folder . "/INBOX.Sent Items")
                   (mu4e-drafts-folder . "/INBOX.Drafts")
                   (mu4e-trash-folder . "/INBOX.Trash")
                   (mu4e-get-mail-command . "offlineimap")
                   (user-email-address . "matt@matthewlemon.com")
                   (user-full-name . "MR Lemon")
                   (smtpmail-default-smtp-server . "mail.messagingengine.com")
                   (smtpmail-smtp-server . "mail.messagingengine.com")
                   (smtpmail-smtp-user . "matthewlemon@fastmail.fm")
                   (smtpmail-smtp-service . "465")
                   (smtpmail-stream-type . ssl)
                   (mu4e-sent-messages-behavior . sent)
                   (mu4e-compose-signature .
                                            (concat
                                             "Matthew Lemon\n"
                                             "Berwick-upon-Tweed"))))))


;; elfeed
(use-package elfeed
  :config
  (setq elfeed-feeds
        '("http://feeds.bbci.co.uk/news/rss.xml?edition=uk"
          "https://www.feedspot.com/?followfeedid=4946040"
          "http://feeds.bbci.co.uk/news/technology/rss.xml"
          "http://newsrss.bbc.co.uk/rss/sportonline_uk_edition/rugby_union/rss.xml"
          "http://feeds.bbci.co.uk/news/video_and_audio/politics/rss.xml"
          "https://feeds.feedburner.com/arstechnica/open-source"
          "https://www.computerweekly.com/rss/IT-security.xml"
          "https://www.fsf.org/static/fsforg/rss/news.xml"
          "https://www.reddit.com/r/freebsd.rss"
          "https://www.reddit.com/r/emacs.rss"
          "https://www.reddit.com/r/rugbyunion/.rss"
          "http://pragmaticemacs.com/feed/"
          "https://200ok.ch/atom.xml"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCkK9UDm_ZNrq_rIXCz3xCGA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCFzGyNKXPAglNq28qWYTDFA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"
          "http://www.linuxinsider.com/perl/syndication/rssfull.pl"
          "http://planet.debian.org/rss20.xml"
          "http://feeds2.feedburner.com/Command-line-fu"
          "https://opensource.org/news.xml"
          "https://feeds.feedburner.com/arstechnica/index"
          "https://www.wired.com/feed/rss"
          "https://sivers.org/en.atom")))

;; recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-exclude '((expand-file-name package-user-dir)
                   ".cache"
                   ".cask"
                   ".elfeed"
                   "bookmarks"
                   "cache"
                   "ido.*"
                   "persp-confs"
                   "recentf"
                   "undo-tree-hist"
                   "url"
                   "COMMIT_EDITMSG\\'"))
    (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
    (recentf-mode t))


;; Basic magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; Integration with pass password manager
(use-package password-store)

;; Git enhancement
(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:modified-sign "|")
  (set-face-foreground 'git-gutter:modified "grey")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")
  :bind (("C-x C-g" . git-gutter))
  :diminish nil)

;; Lisp programming
(use-package paredit
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly))
  :diminish nil)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Ace Jump
(use-package ace-jump-mode
  :bind ("C-M-SPC" . ace-jump-mode))

;; Dump Jump
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy)
  :ensure)

;; GUI stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)


;; Garbage collection
(setq gc-cons-threshold 20000000)

;; org-gcal stuff
(use-package org-gcal
  :bind (:map org-agenda-mode-map ("g" . org-gcal-fetch))
  :config
  (setq org-gcal-secret (password-store-get "EmacsSecrets/org-gcal-secret")
        org-gcal-client-id (password-store-get "EmacsSecrets/org-gcal-client-id"))
  (setq org-gcal-client-secret org-gcal-secret
        org-gcal-file-alist '(("matthew.lemon@gmail.com" .  "~/Nextcloud/org/calendar/work-cal.org")
                            ("12panp3nqdbmm9df4if9jigigo@group.calendar.google.com" .  "~/Nextcloud/org/calendar/home-cal.org"))))

;; No backup files!
(setq make-backup-files nil)

;; Put backups in /tmp where they belong
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Confirm before quitting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; dired config
;; human readable
(setq-default dired-listing-switches "-alh")

;; Dired main config
(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  :config
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

;; recursively copy by default
(setq dired-recursive-copies 'always)

;; y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; auto revert files
(global-auto-revert-mode t)

;; Inhibit splash
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Display the current time
(display-time-mode t)

;; Enable narrow region (disabled by default)
(put 'narrow-to-region 'disabled nil)

;; Enable cursor position when reopening files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Don't lock files
(setq-default create-lockfiles nil)

;; Windmove - use Shift and arrow keys to move in windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Winner mode - undo and redo changes in window config
;; with C-c left and C-c right
(use-package winner
  :ensure nil
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode 1))

;; Don't ring the system bell
(setq visible-bell t)

;; Use a separation file for custom commands
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; Handling tabs (for programming)
(setq-default tab-width 2)
(setq-default tab-width 2 indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq coffee-tab-width 2)
(setq python-indent 2)
(setq css-indent-offset 2)
(add-hook 'sh-mode-hook
	  (lambda ()
	    (setq sh-basic-offset 2
		  sh-indentation 2)))
(setq web-mode-markup-indent-offset 2)

;; flycheck syntax highlighting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Auto-indent with RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Highlight matching parens
(show-paren-mode t)

;; Code folding (hide show mode)
;;- zc: Fold
;;- za: Unfold
;;- zR: Unfold everything
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Line numbers
(add-hook 'prog-mode-hook '(lambda ()
                             (if (version<= emacs-version "26.0.50")
                                 (linum-mode)
                               (display-line-numbers-mode))))

;; Ivy
;; from https://github.com/MatthewZMD/.emacs.d/blob/master/README.md#orgcfd324a
(use-package ivy
  :diminish
  :init
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind
  (("C-s" . swiper-isearch)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-m" . counsel-M-x)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-z s" . counsel-rg)
   ("C-x C-r" . counsel-recentf)
   ("C-z b" . counsel-buffer-or-recentf)
   ("C-z C-b" . counsel-ibuffer)
   (:map ivy-minibuffer-map
         ("C-r" . ivy-previous-line-or-history)
         ("M-RET" . ivy-immediate-done))
   (:map counsel-find-file-map
         ("C-~" . counsel-goto-local-home)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (defun counsel-goto-local-home ()
      "Go to the $HOME of the local machine."
      (interactive)
      (ivy--cd "~/")))

;; Jump to Characters and Words
(use-package avy
  :bind ("M-SPC" . avy-goto-char)
  :config
  (setq avy-background t
        avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

;; Ace Window
(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("M-2" . ace-window))
  :init
  (setq aw-background t
        aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

;; expand-region
(use-package expand-region
  :bind (("C-@" . er/expand-region)
         ("C-=" . er/expand-region)
         ("M-3" . er/expand-region)))

(pending-delete-mode t)

;; writeroom-mode
(use-package writeroom-mode)

;; browse-kill-ring
(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(setq save-interprogram-paste-before-kill t)

;; better re-builder setup
(use-package re-builder
  :bind (("C-c R" . re-builder))
  :config
  (setq reb-re-syntax 'string))

;; reduce size of images in eww
(setq shr-max-image-proportion 0.2)

;; org mode config
(use-package org
  :config
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (global-set-key "\C-cc" 'org-capture)
  (setq org-sort-agenda-notime-is-late nil)
  (setq org-agenda-span 'day)
  (setq org-directory "~/Nextcloud/org")
  (setq org-agenda-files (quote ("~/Nextcloud/org/todo.org"
                                 "~/Nextcloud/org/projects.org"
                                 "~/Nextcloud/org/dft.org"
                                 "~/Nextcloud/org/calendar/home-cal.org"
                                 "~/Nextcloud/org/calendar/work-cal.org")))
  
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq diary-file "~/Nextcloud/org/diary")
  (setq org-agenda-include-diary t)
  (setq org-agenda-show-future-repeats nil)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-reverse-note-order t)
  (setq org-sort-agenda-notime-is-late nil)

  (setq org-archive-location "~/Nextcloud/org/archive.org::* From %s")

  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                   (org-agenda-files :maxlevel . 9))))

  (setq org-agenda-custom-commands
        '(("N" "Agenda and NEXT TODOs" ((agenda "") (todo "NEXT")))
         ("a" "Agenda and All TODOS" ((agenda "") (alltodo "")))
         ("w" "Agenda and WAITING" ((agenda "") (todo "WAITING")))
         ("W" "Agenda and @work" ((agenda "") (tags-todo "@work")))))
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-capture-templates
        (quote (("t" "Templates for Tasks")
                ("tp" "Task Personal" entry (file "~/Nextcloud/org/todo.org")
                 "* TODO %?"
                 :prepend t)
                ("tw" "Task Work" entry (file "~/Nextcloud/org/dft.org")
                 "* TODO %?"
                 :prepend t)
                ("j" "Journal" entry (file+datetree "~/Nextcloud/org/journal.org")
                 "* %?\nEntered on %U\n %i\n %a")
                ("w" "Work Notes and Journaling")
                ("wn" "Note" entry (file+headline "~/Nextcloud/org/dft.org" "Notes")
                 "* %?\n\t")
                ("wc" "Note from Clipboard" entry (file+headline "~/Nextcloud/org/dft.org" "Notes")
                 "* %?\n\t\n%c")
                ("wr" "Note from Region" entry (file+headline "~/Nextcloud/org/dft.org" "Notes")
                 "* %?\n\t\n%i")
                ("wj" "Journal" entry (file+olp+datetree "~/Nextcloud/org/dft.org" "Journal")
                 "* %?\n\tEntered on %U\n")
                ("e" "Emacs Tip")
                ("et" "Emacs Tip" entry (file+headline "~/Nextcloud/org/emacs-tips.org" "Emacs Tips")
                 "* %?\n\t%a")
                ("er" "Emacs Tip from Region" entry (file+headline "~/Nextcloud/org/emacs-tips.org" "Emacs Tips")
                 "* %?\n\t%i")
                )))
  ;; Put state transition logs into a drawer called LOGBOOK
  (setq org-log-into-drawer t)
  
  (setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
              (sequence "PHONE(o)" "MEETING(m)" "PROJECT(p)"))))
  
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PROJECT" :foreground "OrangeRed2" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

  ;; tag stuff automatically dependent on a change of state
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  (setq org-priority-faces
        '((?A . (:foreground "#CC0000" :background "#FFE3E3"))
    (?B . (:foreground "#64992C" :background "#EBF4DD"))
    (?C . (:foreground "#64992C" :background "#FFFFFF"))))
  (setq org-ellipsis "...")
  )

;; org tags
(setq org-tag-alist '(
                      ;; Depth
                      ("@immersive" . ?i) ;; "Deep"
                      ("@process" . ?p) ;; "Shallow"
                      ("@offdesk" . ?o) ;; "Away from desk"
                      ;; Context
                      ("@work" . ?w)
                      ("@home" . ?h)
                      ("@errand" . ?e)
                      ;; Time
                      ("15min" . ?<)
                      ("30min" . ?=)
                      ("1h" . ?>)
                      ;; Energy
                      ("Challenge" . ?1)
                      ("Average" . ?2)
                      ("Easy" . ?3)
                      ;; Misc
                      ("Maybe" . ?m)
                      ))

;; Some nice org speed commands
(setq org-use-speed-commands t
      org-speed-commands-user
      '(("N" org-narrow-to-subtree)
        ("$" org-archive-subtree)
        ("A" org-archive-subtree)
        ("W" widen)
        ("d" org-down-element)
        ("k" org-cut-subtree)
        ("m" org-mark-subtree)
        ("s" org-sort)
        ;; ("x" smex-major-mode-commands)
        ("X" org-todo-done)
        ("R" org-done-and-archive)
        ("y" org-todo-yesterday)))

;; org agenda should be full screen
(defun open-agenda ()
  "Open the org-agenda."
  (interactive)
  (let ((agenda "*Org Agenda*"))
    (if (equal (get-buffer agenda) nil)
        (org-agenda-list)
      (unless (equal (buffer-name (current-buffer)) agenda)
        (switch-to-buffer agenda))
      (org-agenda-redo t)
      (beginning-of-buffer))))

(bind-key "<f5>" 'open-agenda)

;; markdown
(use-package markdown-mode)

;; csvs!
(use-package csv-mode
  :mode ("\\.csv$" . csv-mode))

;; Interactively Do Things (ido)
(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))

;; Which key
(use-package which-key
  :init
  (which-key-mode))


;; Python programming
(use-package elpy
  :ensure py-autopep8
  :defer t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "python3")

  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package python-pytest
  :bind
  ("C-c ESC t" . python-pytest-popup)
  )

;; need this code to fix escape codes in compilation buffer
;; (when running elpy test, for example)
;; from https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Go programming
(use-package lsp-mode
  :hook (go-mode . lsp-deferred)
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package gotest)

;; lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))



;; Yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all)
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))

;; this config works better with yasnippet
(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode ledger-mode) . company-mode)
  :bind
  (:map company-active-map
        ([tab] . smarter-yas-expand-next-field-complete)
        ("TAB" . smarter-yas-expand-next-field-complete))
  :custom
    (company-tooltip-align-annotations t)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  ;; clangd variable not present which was a problem
;;  (unless *clangd* (delete 'company-clang company-backends))
;;  (global-company-mode 1)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (defun smarter-yas-expand-next-field-complete ()
    "Try to `yas-expand' and `yas-next-field' at current cursor position.

If failed try to complete the common part with `company-complete-common'"
    (interactive)
    (if yas-minor-mode
        (let ((old-point (point))
              (old-tick (buffer-chars-modified-tick)))
          (yas-expand)
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (ignore-errors (yas-next-field))
            (when (and (eq old-point (point))
                       (eq old-tick (buffer-chars-modified-tick)))
              (company-complete-common))))
      (company-complete-common))))

;; this stuff from https://youtu.be/XeWZfruRu6k
;; for c programming mainly
(use-package company-irony
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package ledger-mode
  :mode ("\\.ledger\\'")
  :config
  (add-hook 'ledger-mode-hook
            (lambda ()
              (setq-local tab-always-indent 'complete)
              (setq-local completion-cycle-threshold t)
              (setq-local ledger-complete-in-steps t)))
  :custom (ledger-clear-whole-transactions t))

(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)
