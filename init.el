;; Some basic UI stuff
(setq inhibit-startup-message 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(setq visible-bell t)

;; (set-face-attribute 'default nil :font "Jetbrains Mono" :height 140)
(set-face-attribute 'default nil :font "UbuntuMono Nerd Font Mono" :height 160)
;;(load-theme 'tango-dark)
(load-theme 'gruvbox-dark-soft t)

;; calendar proper Monday start
(setq calendar-week-start-day 1)
(setq calendar-date-style (quote european))

;; packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)


(require 'use-package)

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

;; some core bindings
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

;; kill other buffers
(defun kill-other-buffers ()
   "Kill all other buffers."
   (interactive)
   (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :bind ("C-x C-r" . recentf-open-files)
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
        recentf-max-saved-items 50
        recentf-save-file (concat user-emacs-directory ".recentf"))
    (setq recentf-max-menu-items 25)
    (setq recentf-max-saved-items 25)
    (recentf-mode t))

;; encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; beacon mode
(use-package beacon
  :config
  (setq beacon-color "OrangeRed")
  (beacon-mode 1))

;; (use-package ivy
;;   :bind (("C-s" . swiper)
;; 	 :map ivy-minibuffer-map
;; 	 ("TAB" . ivy-alt-done)
;; 	 ("C-l" . ivy-alt-done)
;; 	 ("C-j" . ivy-next-line)
;; 	 ("C-k" . ivy-previous-line)
;; 	 :map ivy-switch-buffer-map
;; 	 ("C-k" . ivy-previous-line)
;; 	 ("C-l" . ivy-done)
;; 	 ("C-d" . ivy-switch-buffer-kill)
;; 	 :map ivy-reverse-i-search-map
;; 	 ("C-k" . ivy-previous-line)
;; 	 ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (ivy-mode 1))

;; Auto completion
(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3
        company-selection-wrap-around t)
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  )
;; (global-company-mode)

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

;; EVIL
;; (use-package evil
;;   :init
;;   :config
;;   (setq evil-respect-visual-line-mode t)
;;   (evil-mode 1))

;; ;; which-key - for nice menu
;; (use-package which-key
;;   :config
;;   (which-key-mode)
;;   )

;; which-key
(use-package which-key
  :config
  (which-key-mode))

;; org mode!
(use-package org
  :config
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (global-set-key "\C-cc" 'org-capture)
  (add-to-list 'org-modules 'org-habit)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-sort-agenda-notime-is-late nil)
  (setq org-agenda-span 'day)
  (setq org-directory "~/org")
  (setq org-agenda-files (quote ("~/org/home.org"
                                 "~/org/projects.org"
                                 "~/org/work.org"
                                 "~/org/habits.org"
				 "~/org/calendar/cal.org"
                                 "~/org/calendar/home-cal.org"
                                 "~/org/calendar/work-cal.org")))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-start-with-log-mode t)
  (setq org-M-RET-may-split-line '(default . nil))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-log-done-with-time 'note)
  (setq diary-file "~/org/diary")
  (setq org-agenda-include-diary t)
  (setq org-agenda-diary-file "~/org/calendar/cal.org")
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-reverse-note-order t)
  (setq org-habit-graph-column 45)
  (setq org-sort-agenda-notime-is-late nil)
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %25TIMESTAMP_IA")
  (setq org-archive-location "~/org/archive.org::* From %s")
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
				   (org-agenda-files :maxlevel . 9))))
  (setq org-agenda-custom-commands
        '(
	  ("N" "Agenda and NEXT TODOs"
	   (
	    (agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT")))
          ("a" "Agenda and All TODOS"
	   ((agenda "")
	    (alltodo "")))
          ("w" "Agenda and WAITING"
	   (
	    (agenda "")
	    (todo "WAITING")))
          ("h" "Agenda and @home"
	   (
	    (agenda "")
	    (tags-todo "@home")
	    ))
	  ("p" "Work Project NEXT"
	   (
	    (agenda "")
	    (tags-todo "+@work+TODO=\"NEXT\"+CATEGORY=\"Project\"" ((org-agenda-overriding-header "Project Tasks")))
	    )
	   )
          ("W" "Agenda and @work"
	   (
	    (agenda "")
	    (tags-todo "@work")
	    ))))
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-capture-templates
        (quote (("t" "Templates for Tasks")
                ("tp" "Task Personal TODO" entry (file+headline "~/org/home.org" "Single Actions")
                 "** TODO %?"
                 :prepend t)
		("tp" "Task Personal NEXT" entry (file+headline "~/org/home.org" "Single Actions")
                 "** NEXT %?"
                 :prepend t)
                ("tw" "Task Work TODO" entry (file+headline "~/org/work.org" "Work Single Actions")
                 "** TODO %?"
                 :prepend t)
                ("tn" "Home Note" entry (file+headline "~/org/home.org" "Notes")
                 "** %?\n\t")
                ("j" "Journal" entry (file+datetree "~/org/journal.org")
                 "* %?\nEntered on %U\n %i\n %a\n %l")
                ("d" "Retrospective Single Action" entry (file+headline "~/org/home.org" "Single Actions")
                 "* DONE %?\nCLOSED: %U")
                ("w" "Work Notes and Journaling")
                ("wn" "Note" entry (file+headline "~/org/work.org" "Notes")
                 "* %?\n\t")
                ("wc" "Note from Clipboard" entry (file+headline "~/org/work.org" "Notes")
                 "* %?\n\t\n%c")
                ("wr" "Note from Region" entry (file+headline "~/org/work.org" "Notes")
                 "* %?\n\t\n%i")
                ("wj" "Journal" entry (file+olp+datetree "~/org/work.org" "Journal")
                 "* %?\n\tEntered on %U\n")
		("i" "Idea" entry (file+headline "~/org/home.org" "Ideas")
		 "** IDEA %?\nEntered on %U\n")
                ("e" "Emacs Tip")
                ("et" "Emacs Tip" entry (file+headline "~/org/emacs-tips.org" "Emacs Tips")
                 "* %?\n\t%a")
                ("er" "Emacs Tip from Region" entry (file+headline "~/org/emacs-tips.org" "Emacs Tips")
                 "* %?\n\t%i")
                )))
  ;; Put state transition logs into a drawer called LOGBOOK
  (setq org-log-into-drawer t)
  
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
		(sequence "IDEA(i)" "|" "BADIDEA(b@/!)")
		(sequence "PHONE(o)" "MEETING(m)" "PROJECT(p)"))))
  
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "cyan" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
		("IDEA" :foreground "yellow" :weight bold)
		("BADIDEA" :foreground "forest green" :weight bold)
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
		      ("@emacs" . ?E)
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

;; elfeed
(use-package elfeed
  :config
  (setq elfeed-feeds
        '(("http://feeds.bbci.co.uk/news/rss.xml?edition=uk" news)
          "https://www.feedspot.com/?followfeedid=4946040"
          ("http://feeds.bbci.co.uk/news/technology/rss.xml" tech news)
          ("https://dominiccummings.com/rss.xml" blog tech)
          ("http://newsrss.bbc.co.uk/rss/sportonline_uk_edition/rugby_union/rss.xml" rugby)
          ("http://feeds.bbci.co.uk/news/video_and_audio/politics/rss.xml" news)
          ("https://feeds.feedburner.com/arstechnica/open-source" opensource)
          ("https://www.computerweekly.com/rss/IT-security.xml" cyber)
          ("https://www.fsf.org/static/fsforg/rss/news.xml" opensource)
          ("https://www.reddit.com/r/freebsd.rss" bsd)
          ("https://www.reddit.com/r/emacs.rss" emacs)
          ("https://www.reddit.com/r/rugbyunion/.rss" rugby)
          ("http://pragmaticemacs.com/feed/" emacs)
          ("https://200ok.ch/atom.xml" emacs)
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCkK9UDm_ZNrq_rIXCz3xCGA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCFzGyNKXPAglNq28qWYTDFA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"
          ("http://www.linuxinsider.com/perl/syndication/rssfull.pl" linux)
          ("http://planet.debian.org/rss20.xml" debian linux)
          ("http://feeds2.feedburner.com/Command-line-fu" linux)
          ("https://opensource.org/news.xml" opensource)
          ("https://feeds.feedburner.com/arstechnica/index" news tech)
          ("https://www.wired.com/feed/rss" news tech)
          ("https://sivers.org/en.atom" blog))))

;; Basic magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".md" ".xml" ".el" ".ini"))
(ido-mode 1)


;; ;; helm
;; (require 'helm-config)
;; (global-set-key (kbd "M-x") #'helm-M-x)
;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
;; ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))
;; (helm-mode 1)


;; ledger mode
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(setq ledger-clear-whole-transactions 1)
(add-to-list 'auto-mode-alist '("\\.ledger%" . ledger-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gruvbox-theme company helm auto-package-update ledger-mode magit elfeed-org which-key use-package rainbow-delimiters paredit evil counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
