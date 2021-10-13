;;; package -- Summary
;; Some basic UI stuff

;;; Commentary:

;;; Code:
(setq inhibit-startup-message 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

;; Put backups in /tmp where they belong
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; recursively copy by default
(setq dired-recursive-copies 'always)

;; y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; auto revert files
(global-auto-revert-mode t)

;; Display the current time
(display-time-mode t)

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

;; start of use-package
(require 'use-package)

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

;; dired config
;; human readable
(setq-default dired-listing-switches "-alh")

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
;;(global-set-key (kbd "C-x C-b") #'ibuffer)
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

;; Highlight matching parens
(show-paren-mode t)

;; Stop C-z suspending emacs
(global-set-key (kbd "C-z") 'nil)

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
  (setq projectile-completion-system 'ivy)
  (defun counsel-goto-local-home ()
      "Go to the $HOME of the local machine."
      (interactive)
      (ivy--cd "~/")))

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

;; browse-kill-ring
(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(setq save-interprogram-paste-before-kill t)


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


;; Ace Jump
(use-package ace-jump-mode
  :bind ("C-M-SPC" . ace-jump-mode))

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
                                 "~/org/notes.org"
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
        '(("w" . "Work")
	        ("wt" "Agenda + Work TODO"
	         (
	          (agenda "")
	          (tags-todo "+@work-SCHEDULED>=\"<today>\""
                       ((org-agenda-overriding-header "Work TODO UNSCHEDULED")))
            (tags-todo "+@work+TODO=\"WAITING\""
                       ((org-agenda-overriding-header "Work WAITING")))
	          ))
	        ("wn" "Agenda + Work NEXT"
	         (
	          (agenda)
	          (tags-todo "+@work+TODO=\"NEXT\"-SCHEDULED>=\"<today>\""
                       ((org-agenda-overriding-header "Work NEXT UNSCHEDULED")))
            (tags-todo "+@work+TODO=\"WAITING\""
                       ((org-agenda-overriding-header "Work WAITING")))
	          ))
	        ("wp" "Work Project NEXT"
	         (
            (agenda)
	          (tags-todo "+@work+TODO=\"NEXT\"+CATEGORY=\"Project\""
                       ((org-agenda-overriding-header "Work Project NEXT actions")))
	          ))
	        ("H" . "Home")
	        ("Hh" "Agenda + Home TODO"
	         (
	          (agenda "")
	          (tags-todo "@home-SCHEDULED>=\"<today>\"-TODO=\"WAITING\""
                       ((org-agenda-overriding-header "Home TODO UNSCHEDULED")
                        (org-agenda-sorting-strategy '(deadline-down scheduled-down priority-down))))
            (tags-todo "+@home+TODO=\"WAITING\""
                       ((org-agenda-overriding-header "Home WAITING")))
	          ))
          ("Hn" "Agenda + Home NEXT"
	         (
	          (agenda "")
	          (tags-todo "+@home+TODO=\"NEXT\"-SCHEDULED>=\"<today\""
                       ((org-agenda-overriding-header "Home NEXT UNSCHEDULED")
                        (org-agenda-sorting-strategy '(deadline-down scheduled-down priority-down))))
            (tags-todo "+@home+TODO=\"DOING\""
                       ((org-agenda-overriding-header "Home DOING")))
	          ))
	        ("n" "Agenda + All NEXT"
	         (
	          (agenda "")
	          (todo "NEXT")))
          ("t" "Agenda + All TODO"
	         (
	          (agenda "")
	          (alltodo "")))
          ("W" "Agenda + All WAITING"
	         (
	          (agenda "")
	          (todo "WAITING")))
          ("i" tags "idea")
          ))
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
	      (quote ((sequence "TODO(t)" "NEXT(n)" "DOING" "|" "DONE(d!)")
		            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
		            )))
  
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "cyan" :weight bold)
                ("DOING" :foreground "orchid" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))

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

;; turn on visual line mode in org mode
(add-hook 'org-mode-hook 'visual-line-mode)

;; org tags
(setq org-tag-alist '(
                      ;; Depth
                      ("@immersive" . ?i) ;; "Deep"
                      ("@process" . ?p)   ;; "Shallow"
                      ("@offdesk" . ?o)   ;; "Away from desk"
                      ;; Type
                      ("brainstorm" . ?b)
                      ("idea" . ?d)
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

(ido-mode 1)
;; Interactively Do Things (ido)
(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".md" ".xml" ".el" ".ini"))
  (setq ido-enable-flex-matching t))


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
(use-package ledger-mode
  :mode ("\\.ledger\\'")
  :config
;;  (setq ledger-default-date-format "%d/%m/%Y")
  (setq ledger-reports
        '(("hsbc_current_account" "ledger [[ledger-mode-flags]] --date-format \"%d/%m/%Y\" -f /home/lemon/Documents/Budget/ledger/2021/budget2021.ledger reg Assets\\:HSBC\\:Current")
          ("bal" "%(binary) -f %(ledger-file) bal")
          ("reg" "%(binary) -f %(ledger-file) reg")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
          ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  (add-hook 'ledger-mode-hook
            (lambda ()
              (setq-local tab-always-indent 'complete)
              (setq-local completion-cycle-threshold t)
              (setq-local ledger-complete-in-steps t)))
  :custom (ledger-clear-whole-transactions t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets yasnippet browse-kill-ring expand-region ace-window amx flycheck ace-jump-mode gruvbox-theme company helm auto-package-update ledger-mode magit elfeed-org which-key use-package rainbow-delimiters paredit evil counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
