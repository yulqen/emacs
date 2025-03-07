;;; --- emacs configuration. -*- lexical-binding: t -*-
;;; package --- MR Lemon emacs config
;;; Commentary:
;;; basic config
;;; Code:

(defvar mrl/startup-time (current-time))
(defun mrl/emacs-load-time ()
  (let ((time (float-time (time-subtract (current-time) mrl/startup-time))))
 "Emacs config loaded in %s seconds"
             (format "%.2f" time))))
(add-hook 'emacs-startup-hook #'mrl/emacs-load-time t)

;; Do I need this for bash commands to work properly?
;; see https://stackoverflow.com/questions/12224909/is-there-a-way-to-get-my-emacs-to-recognize-my-bash-aliases-and-custom-functions
;; (setq shell-file-name "bash")
;; (setq shell-command-switch "-i")

;; define function to shutdown emacs server instance
;; or from outside emacs, do emacsclient -e '(kill-emacs)'
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; timestamps
;; from: https://gist.github.com/takehiko/306021460b21f5d1520c32293cd831e0
(defun mrl/insert-timestamp-default ()
  "Insert the current timestamp"
  (interactive)
  (insert (current-time-string)))

(defun mrl/insert-timestamp-iso ()
  "Insert the current timestamp (ISO 8601 format)"
  (interactive)
  (insert
   (concat
    (format-time-string "%Y-%m-%dT%T")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))

;; uncomment this
(server-start)

;; packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			                   ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; set custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))

;; Don't save the clipboard as this tends to hang
(setq x-select-enable-clipboard-manager nil)

;; easy revert buffer, which I never use
(global-set-key (kbd "C-c R") (lambda () (interactive) (revert-buffer t t)))

;; make sure you require this - otherwise it will not work...
(require 'org-protocol)

;; encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq column-number-mode t
      mode-line-in-non-selected-windows t)

(setq history-length 25)
(savehist-mode 1)

(defun mrl/display-ansi-colors ()
  "Render colors in a buffer that contains ASCII color escape codes."
  (interactive)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook #'mrl/display-ansi-colors)
(add-hook 'eshell-preoutput-filter-functions  #'ansi-color-apply)

(defun mrl/modify-margins ()
  "Add some space around each window."
  (interactive)
  (modify-all-frames-parameters
   '((right-divider-width . 20)
     (internal-border-width . 20)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background)))

(mrl/modify-margins)
;;(add-hook 'ef-themes-post-load-hook 'mrl/modify-margins)

(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)

(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (lambda () (setq-local truncate-lines nil)))
(add-hook 'shell-mode-hook (lambda () (setq-local truncate-lines nil)))

(defun mrl/display-relative-lines ()
  (setq display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'mrl/display-relative-lines)
(add-hook 'yaml-mode-hook #'mrl/display-relative-lines)

(unless (display-graphic-p)
(add-hook 'text-mode-hook #'mrl/display-relative-lines))

(delete-selection-mode t)

(setq compilation-scroll-output 'first-error)

(advice-add 'risky-local-variable-p :override #'ignore)

(setq use-short-answers t)

(setq confirm-kill-emacs 'yes-or-no-p)

(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

(put 'narrow-to-region 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default mark-ring-max 32)
(setq global-mark-ring-max 32)

(setq set-mark-command-repeat-pop t)

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)

(global-set-key (kbd "C-z") #'zap-up-to-char)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(global-set-key [remap list-buffers] 'ibuffer)

(setq completion-styles '(flex basic partial-completion emacs22))

(setq css-indent-offset 2)

;; start-stop emacs
(defun mrl/stop-emacs-1 ()
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(defun mrl/stop-emacs (arg)
  "Close emacs, with a prefix arg restart it.
Restart works only on graphic display."
  (interactive "P")
  (let ((confirm-kill-emacs (unless (and arg (display-graphic-p))
                              'y-or-n-p))
        (kill-emacs-query-functions
         (if (and arg (display-graphic-p))
             (append (list
                      (lambda ()
                        (when (y-or-n-p (format "Really restart %s? "
                                                (capitalize (invocation-name))))
                          (add-hook 'kill-emacs-hook
                                    (lambda ()
                                      (call-process-shell-command
                                       (format "(%s &)"
                                               ;; eselect-emacs.sh
                                               ;; should have kept
                                               ;; only one of
                                               ;; emacs/remacs.
                                               (or (executable-find "emacs")
                                                   (executable-find "remacs")))))
                                    t))))
                     kill-emacs-query-functions)
           kill-emacs-query-functions)))
    (mrl/stop-emacs-1)))

;; notmuch is apparently already installed with notmuch from arch
(require 'notmuch)

;; load my crap
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'mrl-functions)

;; dictionary
(global-set-key (kbd "M-#") #'dictionary-lookup-definition)

(require 'org-notmuch)
(org-link-set-parameters "notmuch"
			                   :follow 'org-notmuch-open
			                   :store 'org-notmuch-store-link)



;; set bookmarks file
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))

(require 'org)
(add-to-list 'org-modules 'org-habit)
;; (org-babel-load-file (expand-file-name "~/.config/emacs/myinit.org"))

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
(menu-bar-mode 0)

(add-hook 'org-mode-hook 'visual-line-mode)


;; turn off flycheck-mode
(add-hook 'org-mode-hook (lambda () flycheck-mode -1))
;; but turn on spelling
;;(add-hook 'org-mode-hook 'flyspell-mode)

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
;;; (global-unset-key (kbd "C-x C-b"))

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
(global-auto-revert-mode)

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

;; (cond
;; ((string-equal (system-name) "schumann")
;;  (when (member "Iosevka Nerd Font" (font-family-list))
;;    (add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font"))))
;; ((string-equal (system-name) "pop_os")
;;  (when (member "Hack" (font-family-list))
;;    (add-to-list 'default-frame-alist '(font . "Hack-10"))))

;; (add-to-list 'default-frame-alist
;;              '(font  . "Iosevka Nerd Font-14"))

(set-frame-font "Iosevka Nerd Font-14")
(defun my-set-default-font ()
  (set-frame-font "Iosevka Nerd Font-14" nil t))

;; Add the function to the `after-init-hook` so it's run after initialization
(add-hook 'after-init-hook 'my-set-default-font)

;; Add the function to the `after-load-theme-hook` so it's run after loading a theme
;;(add-hook 'after-load-theme-hook 'my-set-default-font)

(defun my-apply-font ()
  "Apply my preferred font settings."
  (set-frame-font "Iosevka Nerd Font-14" nil t))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (my-apply-font))))

;; Apply the font settings to the current frame
(my-apply-font)

;; (set-face-attribute 'default nil
;; 		    :family "Iosevka Nerd Font"
;; 		    :height  140)

;; themes by prot
;; (use-package ef-themes
;;   :ensure t)

;;(use-package modus-themes
;;  :ensure t)

;; tsoding's theme
(use-package gruber-darker-theme
  :ensure t)

;; themen
;; (load-theme 'ef-elea-dark t)
;; (load-theme 'gruber-darker t)

;; use this for .envrc files in project directories
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;; org-caldav
(use-package org-caldav
  :config
  (setq org-caldav-url "http://radicale.banded-neon.ts.net/radicale/lemon")
  (setq org-icalendar-timezone "Europe/London")
  ;; (setq org-caldav-calendar-id "7c38e0c7-4a42-9863-c9e0-6025a32c4a65")
  ;; (setq org-caldav-inbox "~/Documents/org/radbox.org")
  ;; (setq org-caldav-files '("~/Documents/org/radcal.org"))
  (setq org-caldav-calendars
        '((:calendar-id "7c38e0c7-4a42-9863-c9e0-6025a32c4a65" :files ("~/Documents/org/radcal.org")
                        :inbox "~/Documents/org/radbox.org")
          (:calendar-id "bb48f855-f7bc-183f-f79d-275327d426d5"
                        :files ("~/Documents/org/radcal_alt.org")
                        :inbox "~/Documents/org/radbox_alt.org")) )
  )
;; Dockerfile syntax highlighting
(use-package dockerfile-mode)

(use-package vterm
  :ensure t)

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
(use-package consult-notes
  :ensure t
  :bind (("C-c d" . consult-notes))
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (setq consult-notes-sources
        '(("Denote"  ?d  "~/Documents/denote/")
          ("Modenote"  ?m  "~/Documents/Notes/MOD/mod-denote/")
          ("Notes archive"  ?n  "~/Documents/Notes/Archive"))) ;; Set notes dir(s), see below
  ;; Set org-roam integration OR denote integration
    (when (locate-library "denote")
  (consult-notes-denote-mode)))

(use-package denote
  :ensure t
  :init
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "~/Documents/denote/"))
  (setq denote-known-keywords '("emacs" "clojure" "org-mode" "work" "technote"))
  (setq denote-file-type 'org)
  (setq denote-prompts '(title keywords))
  (setq denote-date-prompt-use-org-read-date t)
  (require 'denote-journal-extras)

  (defun mrl/denote-find-file ()
    "Find file in the current `denote-directory'."
    (interactive)
    (require 'consult)
    (require 'denote)
    (consult-find (denote-directory)))

  (defun mrl/is-todays-journal? (f)
    "If f is today's journal in denote, f is returned"
    (let* ((month-regexp (car (calendar-current-date)))
           (day-regexp (nth 1 (calendar-current-date)))
           (year-regexp (nth 2 (calendar-current-date)))
           (journal-files (directory-files (denote-directory) nil "_journal"))
           (day-match? (string-match-p (concat "^......" (format "%02d" day-regexp)) f))
           (year-match? (string-match-p (concat "^" (number-to-string year-regexp)) f))
           (month-match? (string-match-p (concat (number-to-string month-regexp) "..T") f)))
      (when (and day-match? year-match? month-match?)
        f)))

  (defvar my-denote-silo-directories
  `("/home/lemon/Documents/Notes/MOD/mod-denote"
    ;; You don't actually need to include the `denote-directory' here
    ;; if you use the regular commands in their global context.  I am
    ;; including it for completeness.
    ,denote-directory)
  "List of file paths pointing to my Denote silos.
  This is a list of strings.")

(defvar my-denote-commands-for-silos
  '(denote
    denote-date
    denote-subdirectory
    denote-template
    denote-type)
  "List of Denote commands to call after selecting a silo.
  This is a list of symbols that specify the note-creating
  interactive functions that Denote provides.")

(defun my-denote-pick-silo-then-command (silo command)
  "Select SILO and run Denote COMMAND in it.
  SILO is a file path from `my-denote-silo-directories', while
  COMMAND is one among `my-denote-commands-for-silos'."
  (interactive
   (list (completing-read "Select a silo: " my-denote-silo-directories nil t)
         (intern (completing-read
                  "Run command in silo: "
                  my-denote-commands-for-silos nil t))))
  (let ((denote-directory silo))
    (call-interactively command)))

  (defun mrl/denote-journal ()
    "Create an entry tagged 'journal' with the date as its title."
    (defvar mrl/in-mod-denote nil)
    (interactive)
    (let* ((journal-dir (concat (denote-directory) "journals"))
           (today-journal
            (car (-non-nil
                  (mapcar #'mrl/is-todays-journal? (directory-files journal-dir nil "_journal"))))))
      (if today-journal
          (find-file (concat journal-dir "/" today-journal))
        (if mrl/in-mod-denote ; this variable is from the .dir-locals.el file in the silo directory; we want to use a specific template
            (denote
             (format-time-string "%A %e %B %Y")
             '("journal") nil journal-dir nil 'modjournal)
          (denote
           (format-time-string "%A %e %B %Y")
           '("journal") nil journal-dir)))))

  :bind (("C-c n n" . denote-create-note)
         ("C-c n d" . mrl/denote-journal)
         ("C-c n t" . denote-type)
         ("C-c n f" . mrl/denote-find-file)
         ("C-c n l" . denote-link))
  )

;; Enable vertico
(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Optionally use the `orderless' completion style.
;; (use-package orderless
;;   :custom
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
;;   ;; (orderless-component-separator #'orderless-escapable-split-on-space)
;;   (completion-styles '(orderless basic))
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles partial-completion)))))



; Example configuration for Consult - from https://github.com/minad/consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind ;; C-c bindings (mode-specific-map)
  ("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c k" . consult-kmacro)
  ;; C-x bindings (ctl-x-map)
  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;; Custom M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ;; M-g bindings (goto-map)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
  ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings (search-map)
  ("M-s d" . consult-find)
  ("M-s D" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  ;;:map isearch-mode-map
  ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
  ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
  ;; Minibuffer history
  ;;:map minibuffer-local-map
  ("M-s" . consult-history)                 ;; orig. next-matching-history-element
  ("M-r" . consult-history)                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  :preview-key (kbd "M-.")
  ;;  :preview-key '(:debounce 0.4 any)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package consult-lsp
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; (use-package undo-tree
;;   :ensure t
;;   :init
;;   (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d./.cache"))) ; from https://github.com/syl20bnr/spacemacs/issues/15426
;;   (global-undo-tree-mode))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))


;; markdown
(use-package markdown-mode
  :ensure t
  :bind (:map markdown-mode-map
              ("C-c C-q" . mrl/clear-check-single-line)
              ("C-c C-v" . mrl/clear-check-from-region))
  :hook (markdown-mode-hook . (lambda ()
                                (when buffer-file-name
                                  (add-hook 'after-save-hook
                                            'check-parens
                                            nil t)))))

;; eglot language server protocol client
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright")))
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"] :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t))))))
  :hook (python-mode . eglot-ensure))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;; Change lsp prefix because at the moment it is "s-l"
;; https://emacs.stackexchange.com/questions/55199/what-are-these-prefix-commands-that-start-with-s-l
(setq lsp-keymap-prefix "C-x @ s-l")


;;; Indication of local VCS changes
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))

;; Enable `diff-hl' support by default in programming buffers
(add-hook 'prog-mode-hook #'diff-hl-mode)

;;; Pop-up completion
(unless (package-installed-p 'corfu)
  (package-install 'corfu))

;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;; Yasnippet
;; (use-package yasnippet
;;   :diminish yas-minor-mode
;;   :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
;;   :bind
;;   (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
;;   (:map yas-keymap
;;         (("TAB" . smarter-yas-expand-next-field)
;;          ([(tab)] . smarter-yas-expand-next-field)))
;;   :config
;;   (use-package yasnippet-snippets)
;;   (yas-reload-all)
;;   (defun smarter-yas-expand-next-field ()
;;     "Try to `yas-expand' then `yas-next-field' at current cursor position."
;;     (interactive)
;;     (let ((old-point (point))
;;           (old-tick (buffer-chars-modified-tick)))
;;       (yas-expand)
;;       (when (and (eq old-point (point))
;;                  (eq old-tick (buffer-chars-modified-tick)))
;;         (ignore-errors (yas-next-field))))))

(add-hook 'after-init-hook 'global-company-mode)

;; this config works better with yasnippet
;; (use-package company
;;   :diminish company-mode
;;   :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode ledger-mode) . company-mode)
;;   :bind
;;   (:map company-active-map
;;         ([tab] . smarter-yas-expand-next-field-complete)
;;         ("TAB" . smarter-yas-expand-next-field-complete))
;;   :custom
;;   (company-tooltip-align-annotations t)
;;   (company-begin-commands '(self-insert-command))
;;   (company-require-match 'never)
;;   ;; Don't use company in the following modes
;;   (company-global-modes '(not shell-mode eaf-mode))
;;   ;; Trigger completion immediately.
;;   (company-idle-delay 0.1)
;;   ;; Number the candidates (use M-1, M-2 etc to select completions).
;;   (company-show-numbers t)
;;   :config
;;   ;; clangd variable not present which was a problem
;;   ;;  (unless *clangd* (delete 'company-clang company-backends))
;;   ;;  (global-company-mode 1)
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 3)
;;   (defun smarter-yas-expand-next-field-complete ()
;;     "Try to `yas-expand' and `yas-next-field' at current cursor position.

;; If failed try to complete the common part with `company-complete-common'"
;;     (interactive)
;;     (if yas-minor-mode
;;         (let ((old-point (point))
;;               (old-tick (buffer-chars-modified-tick)))
;;           (yas-expand)
;;           (when (and (eq old-point (point))
;;                      (eq old-tick (buffer-chars-modified-tick)))
;;             (ignore-errors (yas-next-field))
;;             (when (and (eq old-point (point))
;;                        (eq old-tick (buffer-chars-modified-tick)))
;;               (company-complete-common))))
;;       (company-complete-common))))

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

;; (use-package rainbow-delimiters
;;   :config
;;   (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; which-key
(use-package which-key
  :config
  (which-key-mode))

;; Basic magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; cider
;; (use-package cider
;;   :ensure t)

;; Interactively Do Things (ido)
;; (use-package ido
;;   :config
;;   (ido-mode t)
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-create-new-buffer 'always)
;;   (setq ido-everywhere t)  ; nil because incompatible with Helm
;;   (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".md" ".xml" ".el" ".ini"))
;;   (setq ido-enable-flex-matching t))

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

;; (use-package ivy
;;   :diminish
;;   :init
;;   (use-package amx :defer t)
;;   (use-package counsel :diminish :config (counsel-mode 1))
;;   (use-package swiper :defer t)
;;   (ivy-mode 1)
;;   :bind
;;   (("C-s" . swiper-isearch)
;;    ("C-x C-f" . counsel-find-file)
;;    ("C-x C-m" . counsel-M-x)
;;    ("C-h f" . counsel-describe-function)
;;    ("C-h v" . counsel-describe-variable)
;;    ("C-z s" . counsel-rg)
;;    ("C-x C-r" . counsel-recentf)
;;    ("C-z b" . counsel-buffer-or-recentf)
;;    ("C-z C-b" . counsel-ibuffer)
;;    (:map ivy-minibuffer-map
;;          ("C-r" . ivy-previous-line-or-history)
;;          ("M-RET" . ivy-immediate-done))
;;    (:map counsel-find-file-map
;;          ("C-~" . counsel-goto-local-home)))
;;   :custom
;;   (ivy-use-virtual-buffers t)
;;   (ivy-height 13)
;;   (ivy-on-del-error-function nil)
;;   (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
;;   ;; (ivy-count-format "【%d/%d】")
;;   (ivy-wrap t)
;;   :config
;;   (setq projectile-completion-system 'ivy)
;;   (defun counsel-goto-local-home ()
;;     "Go to the $HOME of the local machine."
;;     (interactive)
;;     (ivy--cd "~/")))

;; beacon mode
(use-package beacon
  :config
  (setq beacon-color "magenta")
  (beacon-mode 1))

;; flycheck syntax highlighting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; turn off flycheck-mode for org
(setq flycheck-global-modes '(not org-mode))

;; install pdf-tools
;;(use-package pdf-tools)
;;(pdf-tools-install)

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

;; remove certain minor modes from the mode line
;;(use-package diminish)

;; ;; elpy for python
;; (use-package elpy
;;   :ensure t
;;   :config
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode)
;;   :init
;;   (elpy-enable))

;; (when (load "flycheck" t t)
;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;; (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Ace Window
;; (use-package ace-window
;;  :bind (("C-x o" . ace-window)
;;         ("M-2" . ace-window))
;;  :init
;;  (setq aw-background t
;;        aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

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
  :bind (("C-x C-r" . recentf-open-files))
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
        recentf-save-file (concat user-config-directory ".recentf"))
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  (recentf-mode t))

(use-package org-rich-yank
  :ensure t
  :demand t
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(use-package org-web-tools
  :ensure t)

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
  ;; advice function to immediately narrow to subtree from org-agenda-goto
  ;; which is hitting TAB to view an item in the agenda
  ;; from https://emacs.stackexchange.com/questions/17797/how-to-narrow-to-subtree-in-org-agenda-follow-mode
  (advice-add 'org-agenda-goto :after
            (lambda (&rest args)
              (org-narrow-to-subtree)))
  (setq org-src-tab-acts-natively t)
  (setq org-directory "~/Documents/org/")
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
  (setq org-M-RET-may-split-line '(default . nil))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-log-done-with-time 'note)
  (setq diary-file "~/Documents/org/diary")
  (setq org-reverse-note-order t)
  (setq org-habit-min-width 55)
  (setq org-habit-show-habits t)
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %25TIMESTAMP_IA")
  (setq org-archive-location "~/Documents/org/archive.org::* From %s")
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

(setq org-agenda-span 'day)
(setq org-agenda-start-day "today")
(setq org-agenda-files (quote ("~/Documents/org/home.org"
                               "~/Documents/org/refile.org"
                               "~/Documents/org/radcal.org"
                               "~/Documents/org/radcal_alt.org"
                               "~/Documents/org/mod.org"
                               "~/Documents/org/calendar/cal.org"
                               "~/Documents/org/habits.org")))
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-include-diary t)
(setq org-agenda-diary-file "~/Documents/org/calendar/cal.org")
(setq org-agenda-show-future-repeats t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
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
          (tags-todo "TODO=\"PROJECT\"" ((org-agenda-overriding-header "Projects")
                                         (org-agenda-sorting-strategy '(alpha-up))))
          (tags-todo "TODO=\"NEXT\"" ((org-agenda-overriding-header "All Next Actions")
                                      (org-agenda-sorting-strategy '(alpha-up deadline-up scheduled-down priority-down))))
          (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "TODO")
                                      (org-agenda-sorting-strategy '(alpha-up)))))
         ((org-agenda-category-filter-preset '("+MOD" "+Proj/Task" "+radcal" "+radcal_alt" "+Meeting" "+WorkTrip" "+refile"))))

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
          (tags "idea" ((org-agenda-overriding-header "Ideas")
                                         (org-agenda-sorting-strategy '(alpha-up))))
          (tags-todo "TODO=\"PROJECT\"" ((org-agenda-overriding-header "Projects")
                                         (org-agenda-sorting-strategy '(alpha-up))))
          (tags-todo "TODO=\"NEXT\"" ((org-agenda-overriding-header "All Next Actions")
                                      (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down))))
          (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "TODO")
                                      (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down)))))
         ((org-agenda-category-filter-preset '("+home" "+habits" "+radcal" "+radcal_alt" "+refile" "+Birthday"))))
        ("i" tags "idea")
        ("r" tags "LEVEL=2+REFILE" ((org-agenda-overriding-header "Stuff to refile")))))

(setq org-capture-templates
      (quote (("i" "Inbox" entry (file+headline "~/Documents/org/refile.org" "Inbox")
               "* %?\nCaptured: %U\n")
              ("h" "Home Tasks & Notes")
              ;; ("w" "Protocol Capture" entry (file+headline "~/org/refile.org" "Web Capture")
              ;;  "* %^{Title or Comment}\nDescription: %:description\nSource: %:link\n%:initial\nCaptured: %U\n")
              ("x" "Protocol Capture" entry (file+headline "~/Documents/org/refile.org" "Web Capture")
               "* TODO Review %:description\nSource: %:link\n%:initial\nCaptured: %U\n" :immediate-finish t)
              ("w" "Protocol Capture" entry (file+headline "~/Documents/org/refile.org" "Web Capture")
               "* %:description\nSource: %:link\n%:initial\nCaptured: %U\n")
              ("ht" "Home TODO" entry (file+headline "~/Documents/org/home.org" "Tasks")
               "** TODO %?\nEntered on %U\n"
               :prepend t)
              ("hn" "Home NEXT" entry (file+headline "~/Documents/org/home.org" "Tasks")
               "** NEXT %?\nEntered on %U\n"
               :prepend t)
              ("hS" "Home Someday" entry (file+headline "~/Documents/org/home.org" "Someday")
               "** SOMEDAY %?\nEntered on %U\n")
              ("hi" "Home Idea" entry (file+headline "~/Documents/org/home.org" "Notes")
               "** %? :idea:\nEntered on %U\n")
              ("hs" "Home Calendar - Single" entry (file+headline "~/Documents/org/home.org" "Calendar")
               "* %?\n%^T")
              ("hb" "Home Calendar - Block" entry (file+headline "~/Documents/org/home.org" "Calendar")
               "* %?\n%^t--%^t")
              ("hr" "Radicale" entry (file+headline "~/Documents/org/radcal.org" "Events")
               "* %?\n%^T")
              ("hR" "Radicale Alt" entry (file+headline "~/Documents/org/radcal_alt.org" "Events")
               "* %?\n%^T")
              ("hD" "Denote Home (org)" plain
               (file denote-last-path)
               #'denote-org-capture
               :no-save t
               :immediate-finish nil
               :kill-buffer t
               :jump-to-captured t)
              ("w" "Work Tasks & Notes")
              ("wt" "Work TODO" entry (file+headline "~/Documents/org/mod.org" "Tasks")
               "** TODO %?\nEntered on %U\n"
               :prepend t)
              ("wn" "Work NEXT" entry (file+headline "~/Documents/org/mod.org" "Tasks")
               "** NEXT %?\nEntered on %U\n"
               :prepend t)
              ("wS" "Work Someday" entry (file+headline "~/Documents/org/mod.org" "Someday")
               "** SOMEDAY %?\nEntered on %U\n")
              ("wN" "Note" entry (file+headline "~/Documents/org/mod.org" "Notes")
               "* %?\nEntered on %U\n")
              ("wc" "Note from Clipboard" entry (file+headline "~/Documents/org/mod.org" "Notes")
               "* %?\n\t\n%c")
              ("wr" "Note from Region" entry (file+headline "~/Documents/org/mod.org" "Notes")
               "* %?\n\t\n%i")
              ("wj" "Journal" entry (file+olp+datetree "~/Documents/org/mod.org" "Journal")
               "* %?\nEntered on %U\n")
              ("wd" "Retrospective Tasks" entry (file+headline "~/Documents/org/mod.org" "Tasks")
               "* DONE %?\nCLOSED: %U")
              ("ws" "Work Calendar - Single" entry (file+headline "~/Documents/org/mod.org" "Calendar")
               "* %?\n%^T")
              ("wb" "Work Calendar - Block" entry (file+headline "~/Documents/org/mod.org" "Calendar")
               "* %?\n%^t--%^t")
              ("wp" "Work Calendar - Trip" entry (file+headline "~/Documents/org/mod.org" "Work Trips")
               "* %?\n%^t--%^t")
              ("wm" "Work Calendar - Meeting" entry (file+headline "~/Documents/org/mod.org" "Meetings")
               "* %?\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n%^T")
              ("wC" "Work Colleague - Block" entry (file+headline "~/Documents/org/mod.org" "Colleagues Calendar")
               "* %?\n%^t--%^t")
              ("e" "Emacs Tip")
              ("et" "Emacs Tip" entry (file+headline "~/Documents/org/emacs-tips.org" "Emacs Tips")
               "* %?\n\t%a")
              ("er" "Emacs Tip from Region" entry (file+headline "~/Documents/org/emacs-tips.org" "Emacs Tips")
               "* %?\n\t%i"))))

(setq org-tag-alist '(
                     ("brainstorm" . ?b)
                     ("idea" . ?d)
                     ("current" . ?C)
                     ("work" . ?w)
                     ("baes" . ?B)
                     ("rrdl" . ?r)
                     ("offscreen" . ?O)
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
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "MediumBlue" :weight bold)
              ("PROJECT" :foreground "blue" :weight bold)
              ("DOING" :foreground "orchid" :weight bold)
              ("DONE" :foreground "ForestGreen" :weight bold)
              ("WAITING" :foreground "black" :background "yellow" :weight bold)
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

(defface org-agenda-radcal-highlight-face `((t :foreground "SpringGreen3"))
    "Face used to highlight radcal entries in agenda view.
https://emacs.stackexchange.com/questions/69564/changing-the-color-of-items-in-org-mode-agenda-depending-on-keyword-tag")

(defface org-agenda-radcal-alt-highlight-face `((t :foreground "dark magenta"))
  "Face used to highlight radcal entries in agenda view.
https://emacs.stackexchange.com/questions/69564/changing-the-color-of-items-in-org-mode-agenda-depending-on-keyword-tag")

(defun org-agenda-highlight-radcal-entries ()
  "Highlight calendar items in agenda."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-begin (line-beginning-position))
              (line-end (line-end-position)))
          (save-excursion
            (goto-char line-begin)
            (when (re-search-forward "radcal_alt" line-end t)
              (font-lock-prepend-text-property line-begin line-end 'face 'org-agenda-radcal-alt-highlight-face))
            (when (re-search-forward "radcal" line-end t)
              (font-lock-prepend-text-property line-begin line-end 'face 'org-agenda-radcal-highlight-face))))
        (forward-line 1)))))

(add-hook 'org-agenda-finalize-hook #'org-agenda-highlight-radcal-entries)
