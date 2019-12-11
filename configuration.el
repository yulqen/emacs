(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(defvar my-packages '(ac-cider
                      ac-js2
                      ag
                      atomic-chrome
                      auto-complete
                      beacon
		      blacken
                      browse-kill-ring
                      cider
                      clj-refactor
                      clojure-mode
                      coffee-mode
                      counsel-jq
                      comment-tags
                      darktooth-theme
                      dired-narrow
                      diminish
                      dumb-jump
                      edit-indirect
                      editorconfig
		      elpy
                      elfeed
                      elfeed-goodies
                      enh-ruby-mode
                      erc-image
                      evil
                      evil-escape
                      evil-leader
                      evil-mc
                      evil-numbers
                      evil-surround
                      exec-path-from-shell
                      forge
                      flycheck
                      flycheck-flow
                      go-mode
                      hide-mode-line
                      ido-vertical-mode
                      impatient-mode
                      ini-mode
                      ivy counsel swiper
                      json-mode
                      js2-mode
                      js2-refactor
                      js-comint
                      ledger-mode
                      lsp-mode
                      lsp-ui
                      lsp-ivy
                      magit
                      markdown-mode
                      package-lint
                      parinfer
                      pdf-tools
                      projectile
		      py-autopep8
		      org
                      rainbow-mode
                      rjsx-mode
                      ob-restclient
                      restclient
                      robe
                      sass-mode
                      spacemacs-theme
                      spaceline
                      smex
                      synosaurus
                      tide
                      visual-fill-column
                      web-mode
                      which-key
                      writegood-mode
                      writeroom-mode
                      yaml-mode
                      zenburn-theme))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p))
  (add-to-list 'package-selected-packages p))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode +1)
(setq savehist-save-minibuffer-history +1)
(setq savehist-additional-vriables
      '(kill-ring
        search-ring
        regexp-search-ring))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Hack-8"))

(if (string< emacs-version
         "26.3")
      (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq gc-cons-threshold 20000000)

(setq make-backup-files nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq vc-follow-symlinks t)

(setq sentence-end-double-space nil)

(setq confirm-kill-emacs 'y-or-n-p)

(put 'dired-find-alternate-file 'disabled nil)

(setq-default dired-listing-switches "-alh")

(setq dired-recursive-copies 'always)

(require 'dired)
(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)

(defun zoom-in ()
  (interactive)
  (let ((x (+ (face-attribute 'default :height)
              10)))
    (set-face-attribute 'default nil :height x)))

(defun zoom-out ()
  (interactive)
  (let ((x (- (face-attribute 'default :height)
              10)))
    (set-face-attribute 'default nil :height x)))

(define-key global-map (kbd "C-1") 'zoom-in)
(define-key global-map (kbd "C-0") 'zoom-out)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(display-time-mode t)

(tool-bar-mode 0)

(add-hook 'org-mode-hook 'auto-fill-mode)

(put 'narrow-to-region 'disabled nil)

(scroll-bar-mode -1)

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(exec-path-from-shell-initialize)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(when (fboundp 'winner-mode)
  (winner-mode 1))

(setq visible-bell t)

(setq ange-ftp-try-passive-mode t)

(add-hook 'eww-mode-hook 'scroll-lock-mode)

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(ac-config-default)

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

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq comment-tags-keymap-prefix (kbd "C-c t"))
(with-eval-after-load "comment-tags"
  (setq comment-tags-keyword-faces
        `(;; A concrete TODO with actionable steps
          ("TODO" . ,(list :weight 'bold :foreground "#DF5427"))
          ;; A non-concrete TODO. We only know something is broken/amiss.
          ("FIXME" . ,(list :weight 'bold :foreground "#DF5427"))
          ;; Works, but is a code smell (quick fix). Might break down the line.
          ("HACK" . ,(list :weight 'bold :foreground "#DF5427"))
          ;; Assumption that needs to be verified.
          ("CHECK" . ,(list :weight 'bold :foreground "#CC6437"))
          ;; Use to highlight a regular, but especially important, comment.
          ("NOTE" . ,(list :weight 'bold :foreground "#1FDA9A"))
          ;; Use to highlight a regular, but especially important, comment.
          ("INFO" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil))
(add-hook 'prog-mode-hook 'comment-tags-mode)

(define-key global-map (kbd "RET") 'newline-and-indent)

(show-paren-mode t)

(add-hook 'before-save-hook '(lambda()
                              (when (not (or (derived-mode-p 'markdown-mode)))
                                (delete-trailing-whitespace))))

(add-hook 'prog-mode-hook #'hs-minor-mode)

(add-hook 'prog-mode-hook '(lambda ()
                             (if (version<= emacs-version "26.0.50")
                                 (linum-mode)
                               (display-line-numbers-mode))))

(elpy-enable)
;;(setq elpy-rpc-python-command "~/.virtualenvs/elpy-rpc/bin/python3")

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq python-shell-interpreter "ptipython"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "ptipython")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'lsp-mode)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook 'lsp-deferred)
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(setq calendar-date-style "european")

;; use fast selection
(setq org-use-fast-todo-selection t)

;; switch state without normal processing
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; basic agenda stuff
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
      (quote (("N" "Agenda and NEXT TODOs"
	       ((agenda "")
		(todo "NEXT")))
	      ("y" "Agenda and All TODOS"
	       ((agenda "")
		(alltodo ""))))))

(define-key global-map "\C-cc" 'org-capture)
(setq org-default-notes-file "~/Nextcloud/org/refile.org")
(setq org-capture-templates
      (quote (("t" "Todo" entry (file "~/Nextcloud/org/todo.org")
               "* TODO %?")
              ("j" "Journal" entry (file+datetree "~/Nextcloud/org/journal.org")
               "* %?\nEntered on %U\n %i\n %a")
              ("e" "Emacs Tip" entry (file+headline "~/Nextcloud/org/emacs-tips.org" "Emacs Tips")
               "* %?\n %i\n %a"))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
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

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)

(setq enable-recursive-minibuffers t)
(global-set-key (kbd "<f6>") 'ivy-resume)

(setq ivy-count-format "(%d/%d) ")

(setq ivy-wrap t)

(global-set-key "\C-s" 'swiper)

(global-set-key (kbd "C-x b") 'counsel-ibuffer)
;; Run `counsel-ag` against the current directory and not against the
;; whole project
(global-set-key (kbd "C-c k") '(lambda()
                                 (interactive)
                                 (counsel-ag "" default-directory nil nil)))
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(global-set-key (kbd "M-x") (lambda ()
                              (interactive)
                              (counsel-M-x "")))

(add-hook 'pdf-view-mode-hook '(lambda()
                                 (define-key pdf-view-mode-map "\C-s" 'isearch-forward)))

(setq projectile-completion-system 'ivy)

(setq mu4e-completing-read-function 'ivy-completing-read)

(setq synosaurus-choose-method 'ivy-read)
