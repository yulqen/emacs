(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(defvar my-packages '(
                      auto-complete
			                blacken
                      browse-kill-ring
                      darktooth-theme
			                elpy
                      elfeed
                      elfeed-goodies
                      evil
                      evil-escape
                      evil-leader
                      evil-mc
                      evil-numbers
                      evil-surround
                      flycheck
                      flycheck-flow
                      helm
                      go-mode
                      impatient-mode
                      ledger-mode
                      lsp-mode
                      lsp-ui
                      magit
                      markdown-mode
                      pdf-tools
                      projectile
			                py-autopep8
			                org
                      rainbow-mode
                      web-mode
                      which-key
                      ))

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
(add-to-list 'default-frame-alist '(font . "Hack-14"))
(load-theme 'manoj-dark)

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
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" "PROJECT"))))

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

(require 'helm-config)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-scroll-amount 5)
(setq helm-ff-file-name-history-use-recentf t)

(evil-mode t)
;; Enable "M-x" in evil mode
(global-set-key (kbd "M-x") 'execute-extended-command)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "w" 'basic-save-buffer
  "s" 'flyspell-buffer
  "b" 'evil-buffer
  "q" 'evil-quit)

(require 'evil-surround)
(global-evil-surround-mode 1)

(global-evil-mc-mode  1)

(define-key evil-normal-state-map (kbd "{") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "}") 'evil-prev-buffer)

(global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C--") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(define-key evil-insert-state-map (kbd "C-v") 'evil-visual-paste)

(mapc (lambda (mode)
        (evil-set-initial-state mode 'emacs)) '(elfeed-show-mode
                                                elfeed-search-mode
                                                forge-pullreq-list-mode
                                                forge-topic-list-mode
                                                dired-mode
                                                help-mode
                                                info
                                                tide-references-mode
                                                image-dired-mode
                                                image-dired-thumbnail-mode
                                                eww-mode))

(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)

(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "jk")
(evil-escape-mode)

(add-hook 'org-mode-hook 'which-key-mode)
(add-hook 'cider-mode-hook 'which-key-mode)

(setq which-key-allow-evil-operators t)
(setq which-key-show-operator-state-maps t)
