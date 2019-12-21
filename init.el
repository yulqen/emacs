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

;; Themes
(use-package monokai-theme)

;; Highlight line
;;(setq set-face-background hl-line-face "pale goldenrod")
;;(hl-line-mode 1)

;; Region colour
(set-face-attribute 'region nil :background "#633" :foreground "#ffffff")

;; Deal with history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode +1)
(setq savehist-save-minibuffer-history +1)
(setq savehist-additional-vriables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Some basics
(setq auto-save-default nil)
(global-set-key (kbd "M-o") 'other-window)
(setq column-number-mode t)
(setq apropos-do-all t)

;; recentf
(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t))

;; Basic magit
(use-package magit
  :bind ("C-x g" . magit-status))

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

;; (use-package highlight-symbol
;;   :config
;;   (add-hook 'prog-mode-hook 'highlight-symbol-mode)
;;   (set-face-background 'highlight-symbol-face "#a45bad")
;;   (set-face-foreground 'highlight-symbol-face "#ffffff")
;;   (setq highlight-symbol-idle-delay 0.5)
;;   :bind (("M-n" . highlight-symbol-next)
;;          ("M-p" . highlight-symbol-prev)))


;; Evil mode
;; (use-package evil
;;   :config
;;   (evil-mode 1)
;;   (mapc (lambda (mode)
;;         (evil-set-initial-state mode 'emacs)) '(elfeed-show-mode
;;                                                 elfeed-search-mode
;;                                                 forge-pullreq-list-mode
;;                                                 forge-topic-list-mode
;;                                                 dired-mode
;;                                                 help-mode
;;                                                 info
;;                                                 tide-references-mode
;;                                                 image-dired-mode
;;                                                 image-dired-thumbnail-mode
;;                                                 eww-mode))

;;   (define-key evil-normal-state-map (kbd "{") 'evil-next-buffer)
;;   (define-key evil-normal-state-map (kbd "}") 'evil-prev-buffer)
;;   )

;; Ace Jump
(use-package ace-jump-mode
  :bind ("C-M-SPC" . ace-jump-mode))

;; Dump Jump
(use-package dumb-jump
  :bind ("C-M-." . dumb-jump-go))

;; GUI stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)


;; org-caldav
(use-package org-caldav
  :config
  ;; (setq org-caldav-url "https://ronver.xyz/remote.php/dav/calendars/mrlemon")
  ;; (setq org-caldav-calendar-id "org-home")
    (setq org-caldav-oauth2-client-id "697419293432-p84jei88v7g6lu9qomk1404o86kim3uj.apps.googleusercontent.com")
  (setq org-caldav-oauth2-client-secret "cwHIENs3tghyb6tEKatxULS_")
  (setq org-caldav-calendar-id "matthew.lemon@gmail.com")
  (setq org-caldav-url 'google)
  (setq org-caldav-sync-direction 'cal->org)
  (setq org-caldav-inbox "~/Nextcloud/org/inbox_cal.org")
  (setq org-caldav-files `("~/Nextcloud/org/cal.org")))

;; fonts
(when (eq system-type 'gnu/linux)
  (set-frame-font "Fira Code Retina 12")
  ;; Default Browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"
        browse-url-new-window-flag t)
  (menu-bar-mode -1)
  ;; enable pdf-tools
  (pdf-tools-install))

;; Garbage collection
(setq gc-cons-threshold 20000000)

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
;; use current buffer with "a" instead of RET
(put 'dired-find-alternate-file 'disabled nil)

;; human readable
(setq-default dired-listing-switches "-alh")

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

;; Windmove - use Shift and arrow keys to move in windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Winner mode - undo and redo changes in window config
;; with C-c left and C-c right
  (when (fboundp 'winner-mode)
    (winner-mode 1))

;; Don't ring the system bell
(setq visible-bell t)
;; Use a separation file for custom commands
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; Basic auto-complete
(use-package auto-complete
  :config
  (ac-config-default))


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
(use-package counsel
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  ;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c L") 'counsel-git-log)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c m") 'counsel-linux-app)
  (global-set-key (kbd "C-c n") 'counsel-fzf)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c J") 'counsel-file-jump)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (global-set-key (kbd "C-c w") 'counsel-wmctrl)
  (setq ivy-height 5)
  )

;; org mode config
(use-package org
  :config
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (global-set-key "\C-cc" 'org-capture)
  (setq org-sort-agenda-notime-is-late nil)
  (setq org-directory "~/Nextcloud/org")
  (setq org-agenda-files '("~/Nextcloud/org"))
  (setq org-default-notes-file (concat org-directory "/refile.org"))
  (setq diary-file "~/Nextcloud/org/diary")
  (setq org-agenda-include-diary t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-reverse-note-order t)
  (setq org-sort-agenda-notime-is-late nil)

  (setq org-archive-location "~/Nextcloud/org/archive.org::* From %s")

  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                   (org-agenda-files :maxlevel . 9))))

  (setq org-agenda-custom-commands
        '(("N" "Agenda and NEXT TODOs" ((agenda "") (todo "NEXT")))
         ("y" "Agenda and All TODOS" ((agenda "") (alltodo "") ))
         ("w" "Agenda and WAITING" ((agenda "") (todo "WAITING")))
         ("p" "Agenda and PROJECTs" ((agenda "") (todo "PROJECT")))))
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-default-notes-file "~/Nextcloud/org/refile.org")
  (setq org-capture-templates
        (quote (("t" "Todo" entry (file "~/Nextcloud/org/todo.org")
                 "* TODO %?")
                ("j" "Journal" entry (file+datetree "~/Nextcloud/org/journal.org")
                 "* %?\nEntered on %U\n %i\n %a")
                ("c" "Calendar entries")
                ("cw" "Work Calendar" entry (file+headline "~/Nextcloud/org/cal.org" "DfT")
                 "* %?\n%^t\n")
                ("ch" "Home Calendar" entry (file+headline "~/Nextcloud/org/cal.org" "Home")
                 "* %?\n%^t\n")
                ("e" "Emacs Tip" entry (file+headline "~/Nextcloud/org/emacs-tips.org" "Emacs Tips")
                 "* %?\n %i\n %a"))))
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

;; Interactively Do Things (ido)
(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))

;; Which key
(use-package which-key
  :hook ((org-mode . which-key-mode)
         (cider-mode . which-key-mode)))

;; Python programming
(use-package elpy
  :ensure py-autopep8
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (setq python-shell-interpreter "ptipython"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "ptipython")

  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; Make sure auto-complete works in python-mode
;; (add-to-list 'ac-modes 'python-mode)

;; Go programming
(use-package lsp-mode
  :hook (go-mode . lsp-deferred)
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :init)

;; Make sure auto-complete works in go-mode
;; (add-to-list 'ac-modes 'go-mode)

(use-package yasnippet
  :commands yas-minor-mode
  :config
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
  :hook (go-mode . yas-minor-mode)
  :hook (python-mode . yas-minor-mode))

(use-package yasnippet-snippets)

;;Company mode is a standard completion package that works well with lsp-mode.
;;company-lsp integrates company mode completion with lsp-mode.
;;completion-at-point also works out of the box but doesn't support snippets.

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-lsp
  :commands company-lsp)

(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)

(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)
