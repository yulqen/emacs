;; Minimise!
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)


;; Stop backup files
(setq make-backup-files nil)

;; Font
(add-to-list 'default-frame-alist '(font . "Fira Code-13"))

;; Packages
(package-initialize)
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(add-to-list
 'package-archives
 '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(add-to-list 'load-path "~/code/elisp/packages/")

;;Helm
(require 'helm-config)
(helm-mode 1)

;; Let's use helm-mini which gives us extras
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)


;; Set switch-buffer key
(global-set-key (kbd "C-,") 'switch-to-buffer)

;; save tonnes of history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode +1)
(setq savehist-save-minibuffer-history +1)
(setq savehist-additional-vriables
      '(kill-ring
        search-ring
        regexp-search-ring))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (gruvbox-dark-medium)))
 '(custom-safe-themes
   (quote
    ("8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default)))
 '(helm-completion-style (quote emacs))
 '(org-sort-agenda-notime-is-late nil t)
 '(package-selected-packages
   (quote
    (yasnippet company-lsp company lsp-ui go-autocomplete lsp-mode go-mode markdown-mode gruvbox-theme helm org cider slime evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; load org settings
;;(load "/home/lemon/.emacs.d/orgsettings.el")

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Org mode set up
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

;;date style
(setq calendar-date-style "european")

;; basic agenda stuff
(setq org-directory "~/Nextcloud/org")
(setq org-agenda-files '("~/Nextcloud/org"))
(setq org-default-notes-file (concat org-directory "/refile.org"))
(setq diary-file "~/Nextcloud/org/diary")
(setq org-agenda-include-diary t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-reverse-note-order t)
(setq org-sort-agenda-notime-is-late nil)

;; use fast selection
(setq org-use-fast-todo-selection t)

;; switch state without normal processing
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Capture shit
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

;; Format string for displaying dates in the daily/weekly agenda
;; and in the timeline.
;;(setq org-agenda-format-date
;;      (concat                         ; "\n"
;;
;;              ;; (make-string (1- (window-width)) (string-to-char "_"))))
;;              (make-string 65 (string-to-char " "))
;;              "_"
;;              ;; (make-string 1 ?\u25AE)
;;              ))

  ;; Faces for specific Priorities (#A, #B and #C).
  (setq org-priority-faces
        '((?A . (:foreground "#CC0000" :background "#FFE3E3"))
          (?B . (:foreground "#64992C" :background "#EBF4DD"))
          (?C . (:foreground "#64992C" :background "#FFFFFF"))))

;; use a single archive file for org
(setq org-archive-location "~/Nextcloud/org/archive.org::* From %s")

;; refiling properly (or generally because everything is included)
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                 (org-agenda-files :maxlevel . 9))))

(setq org-ellipsis "...")
;; END OF ORG MODE STUFF

;; custom commands on agenda buffer
(setq org-agenda-custom-commands
      (quote (("N" "Agenda and NEXT TODOs"
	       ((agenda "")
		(todo "NEXT")))
	      ("y" "Agenda and All TODOS"
	       ((agenda "")
		(alltodo ""))))))

(require 'evil)
(evil-mode 0)
