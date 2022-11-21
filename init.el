;;; package --- MR Lemon emacs config
;;; Commentary:
;;; basic config
;;; Code:

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

;; notmuch is apparently already installed with notmuch from arch
(require 'notmuch)

;; org and notmuch
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'org-notmuch)
(org-link-set-parameters "notmuch"
			 :follow 'org-notmuch-open
			 :store 'org-notmuch-store-link)

;; set custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'org)
(add-to-list 'org-modules 'org-habit)
;; (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

;; Garbage collection
(setq gc-cons-percentage 0.6)

;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (clojure . t)))

;; MISC optimizations
(setq idle-update-delay 1.0)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq-default cursor-in-non-selected-windows nil)
(setq hightlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)
(menu-bar-mode 1)

(add-hook 'org-mode-hook 'visual-line-mode)

;; turn off flycheck-mode
(add-hook 'org-mode-hook (lambda () flycheck-mode -1))

;; ID basics
(setq user-full-name "Matthew Lemon"
      user-mail-address "matt@matthewlemon.com")

;; redundant function
(defun establish-machine ()
  (let ((sys (system-name)))
    (if (string= sys "archdesk")
        (setq mrl/computer 'desktop)
      (setq mrl/computer 'laptop))))
(establish-machine)

;; handier undo
(global-unset-key "\C-z")
(global-set-key "\C-z" 'advertised-undo)

;; UI
(setq inhibit-startup-message 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; we don't want the old buffer list!
(global-unset-key (kbd "C-x C-b")) 

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

;; BACKUPS/LOCKFILES --------
;; Don't generate backups or lockfiles.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/"))))

;; Display the current time
(display-time-mode t)

;; Simply has to be done
(setq visible-bell t)

;; font
(cond
 ((string-equal (system-name) "archdesk")
  (when (member "Hack" (font-family-list))
    (add-to-list 'default-frame-alist '(font . "Hack-14"))))
 ((string-equal (system-name) "pop_os")
  (when (member "Hack" (font-family-list))
    (add-to-list 'default-frame-alist '(font . "Hack-10")))))

;; theme
 (use-package gruber-darker-theme
   :ensure t
   :config
   (load-theme 'gruber-darker t))

 (setq display-line-numbers-type `relative)
 (setq undo-limit 8000000) ; raise limit to 80Mb
 (setq truncate-string-ellipsis "…") ; better than using dots
 (setq scroll-preserve-screen-position 'always) ; experimental
 (setq scroll-margin 3) ; bit of space


 ;; calendar proper Monday start
 (setq calendar-week-start-day 1)
 (setq calendar-date-style (quote european))

 ;; Handling tabs (for programming)
 (setq-default tab-width 2)
 (setq-default tab-width 2 indent-tabs-mode nil)
 (setq-default indent-tabs-mode nil)
 (setq js-indent-level 2)
 (setq python-indent 2)
 (setq css-indent-offset 2)
 (add-hook 'sh-mode-hook
           (lambda ()
             (setq sh-basic-offset 2
                   sh-indentation 2)))
 (setq web-mode-markup-indent-offset 2)

 ;; Highlight matching parens
 (show-paren-mode t)

 ;; Stop C-z suspending emacs
 (global-set-key (kbd "C-z") 'nil)

 ;; encoding
 (prefer-coding-system 'utf-8)
 (setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; PACKAGES

(use-package notmuch
  :defer t
  :config
  (define-key notmuch-show-mode-map "S"
    (lambda ()
      "mark message as spam"
      (interactive)
      (notmuch-show-tag (list "+spam" "-inbox"))))
  (define-key notmuch-search-mode-map "S"
    (lambda ()
      "mark message as spam"
      (interactive)
      (notmuch-search-tag (list "+spam" "-inbox"))))
  (define-key notmuch-search-mode-map "d"
    (lambda ()
      "toggle deleted tag for message"
      (interactive)
      (if (member "deleted" (notmuch-search-get-tags))
          (notmuch-search-tag (list "-deleted"))
        (notmuch-search-tag (list "+deleted")))))
  (setq send-mail-function 'sendmail-send-it
        sendmail-program "/usr/bin/msmtp"
        message-kill-buffer-on-exit t
        notmuch-draft-folder "fastmail/Drafts"
        notmuch-fcc-dirs "fastmail/Sent +sent -unread -inbox"
        notmuch-search-oldest-first t
        mail-specify-envelope-from t
        message-signature "\n\n-- \nMatthew"
        mm-text-html-renderer 'lynx
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        notmuch-saved-searches '((:name "Inbox"
                                        :query "tag:inbox"
                                        :count-query "tag:inbox and tag:unread"
                                        :sort-order newest-first
                                        :key "i")
                                 (:name "Unread"
                                        :query "tag:unread"
                                        :sort-order newest-first
                                        :key "u")
                                 (:name "Sent"
                                        :query "tag:sent"
                                        :sort-order newest-first
                                        :key "s")
                                 (:name "All Mail"
                                        :query "*"
                                        :sort-order newest-first
                                        :key "a")
                                 (:name "Deleted"
                                        :query "tag:deleted"
                                        :sort-order newest-first
                                        :key "d"))))

;; calfw
(use-package calfw-org
  :ensure t
  :config
  (setq cfw:org-agenda-schedule-args '(:timestamp))
  (defun mrl/calf-org-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Orange")))))

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

(use-package deft
  :ensure t
  :config
  (defun mrl/kill-deft ()
      (kill-buffer "*Deft*"))
  (setq deft-directory "~/org-roam"
        deft-extensions '("org" "md" "txt")
        deft-recursive t
        deft-file-limit 40
        deft-use-filename-as-title t)
  (add-hook 'deft-open-file-hook 'mrl/kill-deft))

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

;; ;; EVIL
;;  (use-package evil
;;   :init
;;   :config
;;   (setq evil-respect-visual-line-mode t)
;;   (setq evil-default-state 'emacs)
;;   (evil-mode 0))

;; which-key
(use-package which-key
  :config
  (which-key-mode))

(use-package elfeed
  :config
  (setq elfeed-feeds
        '(("http://feeds.bbci.co.uk/news/rss.xml?edition=uk" news)
          ("http://feeds.bbci.co.uk/news/technology/rss.xml" tech news)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://irreal.org/blog/?feed=rss2" emacs)
          ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-only.xml" emacs)
          ("https://dominiccummings.com/rss.xml" blog tech)
          ("https://usesthis.com/feed.atom" tech blog)
          ("https://plaintextproject.online/feed.xml" plaintext productivity)
          ("https://feeds.feedburner.com/StudyHacks" productivity)
          ("http://newsrss.bbc.co.uk/rss/sportonline_uk_edition/rugby_union/rss.xml" rugby)
          ("http://feeds.bbci.co.uk/news/video_and_audio/politics/rss.xml" news)
          ("https://feeds.feedburner.com/arstechnica/open-source" opensource)
          ("https://www.computerweekly.com/rss/IT-security.xml" cyber)
          ("http://tonsky.me/blog/atom.xml" blog)
          ("https://akkshaya.blog/feed" blob)
          ("https://miguelmota.com/index.xml" blog)
          ("https://www.computerweekly.com/rss/IT-security.xml" security)
          ("https://www.fsf.org/static/fsforg/rss/news.xml" opensource)
          ("https://www.reddit.com/r/emacs.rss" emacs)
          ("https://www.reddit.com/r/rugbyunion/.rss" rugby)
          ("http://pragmaticemacs.com/feed/" emacs)
          ("https://200ok.ch/atom.xml" emacs)
          ("http://www.linuxinsider.com/perl/syndication/rssfull.pl" linux)
          ("http://planet.debian.org/rss20.xml" debian linux)
          ("http://feeds2.feedburner.com/Command-line-fu" linux)
          ("https://opensource.org/news.xml" opensource)
          ("https://www.wired.com/feed/rss" news tech)
          ("https://sivers.org/en.atom" blog))))

;; get scoring in elfeed
(use-package elfeed-score
  :ensure t
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

;; Basic magit
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package cider
  :ensure t)

;; Interactively Do Things (ido)
(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
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
  :ensure t
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
  (ivy-height 13)
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

;; beacon mode
(use-package beacon
  :config
  (setq beacon-color "OrangeRed")
  (beacon-mode 1))

;; flycheck syntax highlighting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; turn off flycheck-mode for org
(setq flycheck-global-modes '(not org-mode))

;; install pdf-tools
(use-package pdf-tools)
(pdf-tools-install)

;; dired
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
;; Ability to use a to visit a new directory or file in dired instead of using RET. RET works just fine,
;; but it will create a new buffer for every interaction whereas a reuses the current buffer.
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-copies 'always)

;; auto-package-update
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

;; Windmove - use Shift and arrow keys to move in windows
;; this fucks around with org mode - we want to shift timestamps and stuff
;;(when (fboundp 'windmove-default-keybindings)
;; (windmove-default-keybindings))

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

;; elpy for python
(use-package elpy
  :ensure t
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  :init
  (elpy-enable))

(when (load "flycheck" t t)
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode))

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

(use-package org-roam
  :ensure t
  :custom
  (org-roam-dailies-directory "daily/")
  (org-roam-directory "~/org-roam")
  (org-roam-capture-ref-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("e" "encrypted" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%T>: %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%A %Y-%m-%d>\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n n" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         :map org-roam-mode-map
         ("y" . org-roam-dailies-goto-previous-note)
         ("t" . org-roam-dailies-goto-next-note)
         ("d" . org-roam-dailies-goto-date)
         ("D" . org-roam-dailies-capture-date))
  :bind-keymap ("C-c n D" . org-roam-mode-map)
  :config
  (defun mrl/search-roam ()
    "Run consult-ripgrep on the org roam directory"
    (interactive)
    (consult-ripgrep org-roam-directory nil))
  (require 'org-roam-protocol)
  (org-roam-db-autosync-mode)
  ;; Bind this to C-c n I
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  :bind (("C-c n I" . org-roam-node-insert-immediate)))

(use-package unicode-fonts
 :ensure t
 :config
  (unicode-fonts-setup))

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

(setq calendar-latitude 55.77)
(setq calendar-longitude -2.01)
(setq calendar-location-name "Berwick-upon-Tweed")

(use-package org
  :init
  (add-to-list 'org-modules 'org-habit)
  :bind (("C-c l" . 'org-store-link)
         ("C-c a" . 'org-agenda)
         ("C-c b" . 'org-iswitchb)
         ("C-c c" . 'org-capture))
  :config
  (setq org-src-tab-acts-natively t)
  (setq org-directory "~/org/")
  (setq org-highest-priority ?A)
  (setq org-default-priority ?C)
  (setq org-lowest-priority ?E)
  (setq org-priority-faces
      '((?A . (:foreground "#CC0000" :background "#FFE3E3"))
        (?B . (:foreground "#64992C" :background "#EBF4DD"))
        (?C . (:foreground "#64992C" :background "#FFFFFF"))))
  (setq org-ellipsis "...")
  (setq org-startup-indented nil)
  (setq org-hide-leading-stars nil)
  (setq org-log-into-drawer t)
  (setq org-deadline-warning-days 4)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-M-RET-may-split-line '(default . nil))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-log-done-with-time 'note)
  (setq diary-file "~/org/diary")
  (setq org-reverse-note-order t)
  (setq +org-habit-min-width 45)
  (setq org-habit-show-habits t)
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %25TIMESTAMP_IA")
  (setq org-archive-location "~/org/archive.org::* From %s")
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

(setq org-agenda-span 'day)
(setq org-agenda-start-day "today")
(setq org-agenda-files (quote ("~/org/home.org"
                               "~/org/projects.org"
                               "~/org/refile.org"
                               "~/org/mod.org"
                               "~/org/notes.org"
                               "~/org/habits.org")))
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file "~/org/calendar/cal.org")
(setq org-agenda-show-future-repeats t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-sort-notime-is-late nil)

(setq org-agenda-custom-commands
      '(
        ("w" "Work"
         (
          (agenda)
          (tags "TODO=\"DOING\"|REFILE+LEVEL=2|current|PRIORITY=\"A\"" ((org-agenda-overriding-header "DEAL")))
          (tags-todo "TODO=\"WAITING\"" ((org-agenda-overriding-header "MOD WAITING")
                                         (org-agenda-sorting-strategy '(deadline-down scheduled-down priority-down))))
          (tags-todo "-SCHEDULED>=\"<today>\"&TODO=\"NEXT\""
                     ((org-agenda-overriding-header "MOD NEXT UNSCHEDULED")
                      (org-agenda-sorting-strategy '(deadline-up priority-down))))
          (tags-todo "TODO=\"PROJECT\"" ((org-agenda-overriding-header "Projects")))
          (tags-todo "TODO=\"NEXT\"" ((org-agenda-overriding-header "All Next Actions")
                                      (org-agenda-sorting-strategy '(deadline-up scheduled-down priority-down))))
          (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "TODO")
                                      (org-agenda-sorting-strategy '(deadline-up)))))
         ((org-agenda-category-filter-preset '("+MOD" "+Proj/Task" "+Meeting" "+WorkTrip" "+refile"))))

        ("h" "Home"
         (
          (agenda)
          (tags "TODO=\"DOING\"|REFILE+LEVEL=2|current|PRIORITY=\"A\"" ((org-agenda-overriding-header "DEAL")
                                                                        (org-agenda-sorting-strategy '(priority-down alpha-up))))
          (tags-todo "TODO=\"WAITING\"" ((org-agenda-overriding-header "Home WAITING")
                                         (org-agenda-sorting-strategy '(deadline-down scheduled-down priority-down))))
          (tags-todo "-SCHEDULED>=\"<today>\"&TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Home NEXT UNSCHEDULED")
                      (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down))))
          (tags-todo "TODO=\"PROJECT\"" ((org-agenda-overriding-header "Projects")
                                         (org-agenda-sorting-strategy '(alpha-up))))
          (tags-todo "TODO=\"NEXT\"" ((org-agenda-overriding-header "All Next Actions")
                                      (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down))))
          (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "TODO")
                                      (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down)))))
         ((org-agenda-category-filter-preset '("+home" "+habits" "+refile"))))
        ("i" tags "idea")
        ("r" tags "LEVEL=2+REFILE" ((org-agenda-overriding-header "Stuff to refile")))))

(setq org-capture-templates
      (quote (("i" "Inbox" entry (file+headline "~/org/refile.org" "Inbox")
               "* %?\nCaptured: %U\n")
              ("h" "Home Tasks & Notes")
              ;; ("w" "Protocol Capture" entry (file+headline "~/org/refile.org" "Web Capture")
              ;;  "* %^{Title or Comment}\nDescription: %:description\nSource: %:link\n%:initial\nCaptured: %U\n")
              ("x" "Protocol Capture" entry (file+headline "~/org/refile.org" "Web Capture")
               "* TODO Review %:description\nSource: %:link\n%:initial\nCaptured: %U\n" :immediate-finish t)
              ("w" "Protocol Capture" entry (file+headline "~/org/refile.org" "Web Capture")
               "* %:description\nSource: %:link\n%:initial\nCaptured: %U\n")
              ("ht" "Home TODO" entry (file+headline "~/org/home.org" "Tasks")
               "** TODO %?\nEntered on %U\n"
               :prepend t)
              ("hn" "Home NEXT" entry (file+headline "~/org/home.org" "Tasks")
               "** NEXT %?\nEntered on %U\n"
               :prepend t)
              ("hS" "Home Someday" entry (file+headline "~/org/home.org" "Someday")
               "** SOMEDAY %?\nEntered on %U\n")
              ("hi" "Home Idea" entry (file+headline "~/org/home.org" "Notes")
               "** %? :idea:\nEntered on %U\n")
              ("hs" "Home Calendar - Single" entry (file+headline "~/org/home.org" "Calendar")
               "* %?\n%^T")
              ("hb" "Home Calendar - Block" entry (file+headline "~/org/home.org" "Calendar")
               "* %?\n%^t--%^t")
              ("w" "Work Tasks & Notes")
              ("wt" "Work TODO" entry (file+headline "~/org/mod.org" "Tasks")
               "** TODO %?\nEntered on %U\n"
               :prepend t)
              ("wn" "Work NEXT" entry (file+headline "~/org/mod.org" "Tasks")
               "** NEXT %?\nEntered on %U\n"
               :prepend t)
              ("wS" "Work Someday" entry (file+headline "~/org/mod.org" "Someday")
               "** SOMEDAY %?\nEntered on %U\n")
              ("wN" "Note" entry (file+headline "~/org/mod.org" "Notes")
               "* %?\nEntered on %U\n")
              ("wc" "Note from Clipboard" entry (file+headline "~/org/mod.org" "Notes")
               "* %?\n\t\n%c")
              ("wr" "Note from Region" entry (file+headline "~/org/mod.org" "Notes")
               "* %?\n\t\n%i")
              ("wj" "Journal" entry (file+olp+datetree "~/org/mod.org" "Journal")
               "* %?\nEntered on %U\n")
              ("wd" "Retrospective Tasks" entry (file+headline "~/org/mod.org" "Tasks")
               "* DONE %?\nCLOSED: %U")
              ("ws" "Work Calendar - Single" entry (file+headline "~/org/mod.org" "Calendar")
               "* %?\n%^T")
              ("wb" "Work Calendar - Block" entry (file+headline "~/org/mod.org" "Calendar")
               "* %?\n%^t--%^t")
              ("wp" "Work Calendar - Trip" entry (file+headline "~/org/mod.org" "Work Trips")
               "* %?\n%^t--%^t")
              ("wm" "Work Calendar - Meeting" entry (file+headline "~/org/mod.org" "Meetings")
               "* %?\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n%^T")
              ("e" "Emacs Tip")
              ("et" "Emacs Tip" entry (file+headline "~/org/emacs-tips.org" "Emacs Tips")
               "* %?\n\t%a")
              ("er" "Emacs Tip from Region" entry (file+headline "~/org/emacs-tips.org" "Emacs Tips")
               "* %?\n\t%i"))))

(setq org-tag-alist '(
                     ;; Type
                     ("brainstorm" . ?b)
                     ("idea" . ?d)
                     ;; Context
                     ("work" . ?w)
                     ("computer" .?c)
                     ("home" . ?h)
                     ("errand" . ?e)
                     ("emacs" . ?E)
                     ("orgmode" . ?o)
                     ("joanna" . ?j)
                     ("harvey" . ?H)
                     ("sophie" . ?S)))

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

(setq org-stuck-projects
      '("+LEVEL=2/+PROJECT" ("NEXT" "DOING") nil ""))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DEFERRED(r@/!)")
              (sequence "TODO(t)" "NEXT(n)" "DOING(D)" "PROJECT(p)"  "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(s@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "tomato4" :weight bold)
              ("NEXT" :foreground "LemonChiffon" :weight bold)
              ("PROJECT" :foreground "ForestGreen" :weight bold)
              ("DOING" :foreground "brown" :weight bold)
              ("DONE" :foreground "SlateGray" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("SOMEDAY" :foreground "blue" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "snow4" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))))
