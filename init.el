(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; set custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; require separate config files
(require 'fontaine)
(require 'mrl-functions)
(require 'denote-stuff)
(require 'org-babel-config)
(require 'theme-config)
(require 'font-config)

;; use org mode as scratch buffer
;;(setq initial-major-mode 'org-mode)

;;; Enable the Emacs server, allows editing via emacsclient
(require 'server)
(when (not (server-running-p)) (server-start))

;; match PATH
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			                    "[ \t\n]*$" "" (shell-command-to-string
					                                "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(require 'use-package)
(setq use-package-always-ensure t)

;; (use-package gruber-darker-theme)
;; (use-package borland-blue-theme)
;; (use-package autumn-light-theme)

;; ;; Install SLIME if not already installed
;; (unless (package-installed-p 'slime)
;;   (package-refresh-contents) ; Refresh package list
;;   (package-install 'slime))

;; use arrows to go back and forth
(use-package backward-forward
  :ensure t
  :demand t
  :config
  (backward-forward-mode t)
  :bind
  (:map backward-forward-mode-map
        ("<left>" . backward-forward-previous-location)
        ("<right>" . backward-forward-next-location)))

;; Configure SLIME
(use-package slime
  :ensure t
  :init
  ;; Load the quicklisp-slime-helper if you use Quicklisp
  (load (expand-file-name "~/quicklisp/slime-helper.el"))

  :config
  ;; Set SBCL as the inferior Lisp program
  (setq inferior-lisp-program "sbcl")

  ;; Load SLIME contrib modules for extended functionality
  (setq slime-contribs '(slime-fancy   ; comprehensive set of features
                         slime-quicklisp ; Quicklisp integration
                         slime-asdf      ; ASDF integration
                         slime-mrepl     ; multiple REPLs
                         ;; Add other contribs as needed, e.g.,
                         ;; slime-autodoc
                         ;; slime-editing-commands
                         ))
  (slime-setup slime-contribs)

  ;; Optional: Enable paredit for structural editing of Lisp code
  (autoload 'paredit-mode "paredit" "Minor mode for structural editing of Lisp code." t)
  (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
  )

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-global-modes '(not org-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js-ts-mode))

(use-package flycheck-clj-kondo
  :hook (after-init . global-flycheck-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Enable Vertico.
(use-package vertico
  :ensure t
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  (completion-cycle-threshold 3) ;; see corfu config
  (tab-always-indent 'complete) ;; see corfu config
  (text-mode-ispell-word-completion nil) ;; see corfu config
  ;; hide commands in M-x which not apply to the current mode
  ;; Got this from corfu docs but suggests it is useful elsewhere
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

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

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

(use-package cider
  :ensure t
  :hook ((cider-repl-mode . paredit-mode)
         (clojure-mode . eglot-ensure)) ; Added from your clojure-mode hook
  :config
  
  (setq cider-jack-in-default 'clojure-cli)
  (setq nrepl-use-ssh-fallback-for-remote-hosts t))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . eglot-ensure)
         (clojure-mode . paredit-mode)))

(use-package flycheck-clj-kondo
  :ensure t
  :hook (clojure-mode . flycheck-mode))

(use-package parseedn)

(setq treesit-language-source-alist
      '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
        (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))


(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-show-minibuffer-help t)
  (setq undo-tree-minibuffer-help-dynamic t))

(use-package gptel
  :config
  (setq gptel-include-reasoning nil)
  (setq gptel-default-mode 'org-mode)
  (gptel-make-anthropic "Claude" :stream t :key gptel-api-key)
  (gptel-make-gemini "Gemini" :stream t :key gptel-api-key)
  (setq gptel-api-key 'gptel-api-key-from-auth-source)
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(openai/gpt-3.5-turbo
             mistralai/mixtral-8x7b-instruct
             meta-llama/codellama-34b-instruct
             codellama/codellama-70b-instruct
             google/palm-2-codechat-bison-32k
             google/gemini-pro))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(llama3.1:latest deepseek-r1:latest)))

(use-package paredit
  :hook
  (clojure-mode                     . paredit-mode) ; Clojure buffers
  (emacs-lisp-mode                  . paredit-mode) ; Elisp buffers.
  (lisp-mode                        . paredit-mode) ; Common Lisp buffers.
  (lisp-interaction-mode            . paredit-mode) ; Scratch buffers.
  (ielm-mode-hook                   . paredit-mode) ; ELM buffers.
  (eval-expression-minibuffer-setup . paredit-mode) ; Eval minibuffers.
  :bind
  (:map paredit-mode-map
        ("<return>" . my/paredit-RET))
  :config
  (defun my/paredit-RET ()
    "Wraps `paredit-RET' to provide a sensible minibuffer experience."
    (interactive)
    (if (minibufferp)
        (read--expression-try-read)
      (paredit-RET))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package simpc-mode
  :ensure nil
  :load-path "lisp/"
  :mode ("\\.h\\(pp\\)?\\'" . simpc-mode)
  :mode ("\\.c\\(pp\\)?\\'" . simpc-mode))


(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-\"" . mc/skip-to-next-like-this)
         ("C-:" . mc/skip-to-previous-like-this)))

(use-package dired-x
  :ensure nil
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$"))
  (setq-default dired-dwim-target t)
  (setq dired-listing-switches "-alh")
  (setq dired-mouse-drag-files t))

(use-package web-mode
  :ensure t
  :mode (("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :hook (web-mode . eglot-ensure)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-indentation t))

(use-package treesit
  :ensure nil
  :config
  ;; This is the correct format: (LANG . (URL [REVISION] [SUBDIR]))
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :hook
  ;; Use the modern *-ts-mode for languages that have it
  ((js-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (python-mode . python-ts-mode)))

(use-package emmet-mode
  :ensure t
  :hook ((web-mode css-mode sgml-mode html-mode) . emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))


;; eww as default browser
(setq browse-url-browser-function 'eww-browse-url)

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

(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )

;; --- PYTHON USE-PACKAGE CONFIGURATION ---
(use-package python
  :ensure nil ;; This is a built-in feature
  ;; The :mode keyword is the trigger. It tells use-package to create an
  ;; autoload that says "when a .py file is opened, use python-ts-mode".
  ;; Because we are configuring the 'python' feature itself, this
  ;; reliably overrides the default.
  :mode ("\\.py\\'" . python-ts-mode)
  :config
  ;; This :config block will now run correctly when python-ts-mode is loaded.
  (setq python-indent-offset 2)
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . pyvenv-mode)
         (python-ts-mode . flycheck-mode))
  :bind (:map python-ts-mode-map
              ("C-c t p" . mrl/run-python-tests-for-project)
              ("C-c t a" . mrl/run-python-tests-for-app)
              ("C-c t b" . mrl/run-python-tests-in-buffer)
              ("C-c t f" . mrl/run-python-test-at-point)))

;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)))

(use-package pyvenv
  :ensure t
  :hook (python-ts-mode . (lambda ()
                            (let ((venv-dir (expand-file-name ".venv" (project-current))))
                              (when (file-directory-p venv-dir)
                                (pyvenv-activate venv-dir))))))


;; (use-package django-mode)
(use-package django-snippets)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("C-c y" . yas-expand))
  :config
  ;; Your config here
  :init
  (yas-global-mode 1))


(use-package yasnippet-snippets)
(use-package clojure-snippets)


(use-package direnv
  :config
  (direnv-mode))

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package beacon
  :ensure t
  :hook (prog-mode . beacon-mode))

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

(use-package dockerfile-mode)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package diminish
  :config
  (diminish 'completion-preview-mode)
  (diminish 'which-key-mode)
  (diminish 'beacon-mode))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-show-numbers t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-global-modes '(not org-mode)))

(with-eval-after-load 'completion-preview
  ;; Show the preview already after two symbol characters
  (setq completion-preview-minimum-symbol-length 2)

  ;; Non-standard commands to that should show the preview:

  ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands)
  ;; Paredit has a custom `delete-backward-char' command
  (push 'paredit-backward-delete completion-preview-commands)

  ;; Bindings that take effect when the preview is shown:

  ;; Cycle the completion candidate that the preview shows
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

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

;; ID basics
(setq user-full-name "Matthew Lemon"
      user-mail-address "matt@matthewlemon.com")

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


;; recursively copy by default
(setq dired-recursive-copies 'always)

;; y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; auto revert files
(global-auto-revert-mode)

(setq make-backup-files nil          ; don't create backup files
      create-lockfiles nil           ; don't create lockfiles
      auto-save-default nil          ; don't auto-save to #file#
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))) ; but if auto-save is on, put it in /tmp

;; Display the current time
(display-time-mode t)

;; Simply has to be done
(setq visible-bell t)

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

(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode))

;; turn off flycheck-mode for org
(setq flycheck-global-modes '(not org-mode))

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

(setq calendar-latitude 55.77)
(setq calendar-longitude -2.01)
(setq calendar-location-name "Berwick-upon-Tweed")

;; org-mode
;; org-crypt stuff from https://orgmode.org/manual/Org-Crypt.html
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt" "current"))


(setq org-crypt-key nil)
;; GPG key to use for encryption.
;; nil means  use symmetric encryption unconditionally.
;; "" means use symmetric encryption unless heading sets CRYPTKEY property.

(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-
;; END OF org-crypt config

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(add-to-list 'org-modules 'org-habit)
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
                               "~/Documents/org/radbox.org"
                               "~/Documents/org/radbox_alt.org"
                               "~/Documents/org/radcal_coding.org"
                               "~/Documents/org/radbox_work.org"
                               "~/Documents/org/dft.org"
                               "~/Documents/org/alphabet_learning.org"
                               "~/Documents/org/calendar/cal.org"
                               "~/Documents/org/habits.org")))
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-include-diary t)
(setq org-agenda-diary-file "~/Documents/org/diary")
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
          (tags-todo "TODO=\"NEXT\"" ((org-agenda-overriding-header "All Next Actions")
                                      (org-agenda-sorting-strategy '(alpha-up deadline-up scheduled-down priority-down))))
          ;; (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "TODO")
          ;;                             (org-agenda-sorting-strategy '(alpha-up))))
          (tags-todo "TODO=\"WAITING\"" ((org-agenda-overriding-header "DfT WAITING")
                                         (org-agenda-sorting-strategy '(deadline-down scheduled-down priority-down))))
          (tags-todo "-SCHEDULED>=\"<today>\"&TODO=\"NEXT\""
                     ((org-agenda-overriding-header "DfT NEXT UNSCHEDULED")
                      (org-agenda-sorting-strategy '(deadline-up priority-down))))
          (tags-todo "TODO=\"PROJECT\"" ((org-agenda-overriding-header "Projects")
                                         (org-agenda-sorting-strategy '(alpha-up))))
          )
         ((org-agenda-category-filter-preset '("+DfT" "+Proj/Task" "+radbox" "+radbox_alt" "+radbox_work" "+radbox_coding" "+Meeting" "+WorkTrip" "+refile"))))

        ("c" "Central Project Register"
         (
          (tags "TODO=\"DOING\"|REFILE+LEVEL=2|current|PRIORITY=\"A\"" ((org-agenda-overriding-header "DEAL")))
          (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "Tasks")
                                      (org-agenda-sorting-strategy '(alpha-up))))
          (tags-todo "TODO=\"WAITING\"" ((org-agenda-overriding-header "CPR Waiting/Blocked")
                                         (org-agenda-sorting-strategy '(deadline-down scheduled-down priority-down))))
          (tags-todo "-SCHEDULED>=\"<today>\"&TODO=\"NEXT\""
                     ((org-agenda-overriding-header "DfT NEXT UNSCHEDULED")
                      (org-agenda-sorting-strategy '(deadline-up priority-down))))
          )
         ((org-agenda-category-filter-preset '("+CPR-Tasks"))))
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
          (tags-todo "TODO=\"NEXT\"" ((org-agenda-overriding-header "All Next Actions")
                                      (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down))))
          ;; (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "TODO")
          ;;                             (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down))))
          (tags "idea" ((org-agenda-overriding-header "Ideas")
                        (org-agenda-sorting-strategy '(alpha-up))))
          (tags-todo "TODO=\"PROJECT\"" ((org-agenda-overriding-header "Projects")
                                         (org-agenda-sorting-strategy '(alpha-up)))))
         ((org-agenda-category-filter-preset '("+home" "+habits" "+radbox" "+radbox_alt" "+radbox_work" "+radbox_coding" "+refile" "+Birthday"))))
        ("A" "Alphabet Learning Project"
         (
          (tags-todo "TODO=\"DOING\"" ((org-agenda-overriding-header "Doing")
                                       (org-agenda-sorting-strategy '(priority-down tag-up alpha-up deadline-down scheduled-down))))
          (tags-todo "TODO=\"NEXT\"" ((org-agenda-overriding-header "Next")
                                      (org-agenda-sorting-strategy '(priority-down tag-up alpha-up deadline-done schedule-down))))
          (tags "bug" ((org-agenda-overriding-header "Bugs")
                       (org-agenda-sorting-strategy '(priority-down alpha-up))))
          (tags "ui" ((org-agenda-overriding-header "UI")
                      (org-agenda-sorting-strategy '(priority-down alpha-up))))
          (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "Tasks")
                                      (org-agenda-sorting-strategy '(priority-down tag-up alpha-up deadline-down scheduled-down))))
          )
         ((org-agenda-category-filter-preset '("+AL"))))
        ("i" tags "idea")
        ("r" tags "LEVEL=2+REFILE" ((org-agenda-overriding-header "Stuff to refile")))))

(setq org-capture-templates
      (quote (("i" "Inbox" entry (file+headline "~/Documents/org/refile.org" "Inbox")
               "* %?\nCaptured: %U\n")
              ("a" "Alphabet Learning Tasks")
              ("at" "Alphabet Learning TODO" entry (file+headline "~/Documents/org/alphabet_learning.org" "Tasks")
               "** TODO %?\nEntered on %U\n"
               :prepend t)
              ("ab" "Alphabet Learning TODO" entry (file+headline "~/Documents/org/alphabet_learning.org" "Tasks")
               "** TODO %? :bug:\nEntered on %U\n"
               :prepend t)
              ("h" "Home Tasks & Notes")
              ;; ("w" "Protocol Capture" entry (file+headline "~/org/refile.org" "Web Capture")
              ;;  "* %^{Title or Comment}\nDescription: %:description\nSource: %:link\n%:initial\nCaptured: %U\n")
              ;; ("x" "Protocol Capture" entry (file+headline "~/Documents/org/refile.org" "Web Capture")
              ;;  "* TODO Review %:description\nSource: %:link\n%:initial\nCaptured: %U\n" :immediate-finish t)
              ;; ("w" "Protocol Capture" entry (file+headline "~/Documents/org/refile.org" "Web Capture")
              ;;  "* %:description\nSource: %:link\n%:initial\nCaptured: %U\n")

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
              ("hn" "Note" entry (file+headline "~/Documents/org/home.org" "Notes")
               "** %?\nEntered on %U\n")
              ("hw" "Quick Note (Web link)" entry (file+headline "~/Documents/org/home.org" "Notes")
               "** %? :quicknote:\nCaptured on: %U\nSource: %x")
              ("hj" "Journal" entry (file+olp+datetree "~/Documents/org/home.org" "Journal")
               "* %U: %?\n")
              ("hs" "Home Calendar - Single" entry (file+headline "~/Documents/org/home.org" "Calendar")
               "* %?\n%^T")
              ("hb" "Home Calendar - Block" entry (file+headline "~/Documents/org/home.org" "Calendar")
               "* %?\n%^t--%^t")
              ("hr" "Radicale" entry (file+headline "~/Documents/org/radcal.org" "Events")
               "* %?\n%^T")
              ("hR" "Radicale Alt" entry (file+headline "~/Documents/org/radcal_alt.org" "Events")
               "* %?\n%^T")
              ("hC" "Radicale Coding" entry (file+headline "~/Documents/org/radcal_coding.org" "Events")
               "* %?\n%^T")         
              ("hD" "Denote Home (org)" plain
               (file denote-last-path)
               #'denote-org-capture
               :no-save t
               :immediate-finish nil
               :kill-buffer t
               :jump-to-captured t)
              ("hJ" "Denote Journal" entry (file denote-journal-path-to-new-or-existing-entry)
               "* %U %?\n%i"
               :kill-buffer t
               :empty-lines 1)
              ("w" "Work Tasks & Notes")
              ("wt" "Work TODO" entry (file+headline "~/Documents/org/dft.org" "Tasks")
               "** TODO %?\nEntered on %U\n"
               :prepend t)
              ("wn" "Work NEXT" entry (file+headline "~/Documents/org/dft.org" "Tasks")
               "** NEXT %?\nEntered on %U\n"
               :prepend t)
              ("wS" "Work Someday" entry (file+headline "~/Documents/org/dft.org" "Someday")
               "** SOMEDAY %?\nEntered on %U\n")
              ("wN" "Note" entry (file+headline "~/Documents/org/mod.org" "Notes")
               "* %?\nEntered on %U\n")
              ("wc" "Note from Clipboard" entry (file+headline "~/Documents/org/dft.org" "Notes")
               "* %?\n\t\n%c")
              ("wr" "Note from Region" entry (file+headline "~/Documents/org/dft.org" "Notes")
               "* %?\n\t\n%i")
              ("wj" "Journal" entry (file+olp+datetree "~/Documents/org/dft.org" "Journal")
               "* %?\nEntered on %U\n")
              ("wd" "Retrospective Tasks" entry (file+headline "~/Documents/org/dft.org" "Tasks")
               "* DONE %?\nCLOSED: %U")
              ("ws" "Work Calendar - Single" entry (file+headline "~/Documents/org/dft.org" "Calendar")
               "* %?\n%^T")
              ("wb" "Work Calendar - Block" entry (file+headline "~/Documents/org/dft.org" "Calendar")
               "* %?\n%^t--%^t")
              ("wp" "Work Calendar - Trip" entry (file+headline "~/Documents/org/dft.org" "Work Trips")
               "* %?\n%^t--%^t")
              ("wm" "Work Calendar - Meeting" entry (file+headline "~/Documents/org/dft.org" "Meetings")
               "* %?\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n%^T")
              ("wC" "Work Colleague - Block" entry (file+headline "~/Documents/org/dft.org" "Colleagues Calendar")
               "* %?\n%^t--%^t")
              ("e" "Tech Tip")
              ("et" "Emacs Tip" entry (file+headline "~/Documents/org/tech-tips.org" "Emacs Tips")
               "* %?\n\t%a")
              ("er" "Emacs Tip from Region" entry (file+headline "~/Documents/org/tech-tips.org" "Emacs Tips")
               "* %?\n\t%i"))))

;; Commented these out because I now have a tags.org file and use #+SETUPFILE: in home.org, etc
;; (setq org-tag-alist '(
;;                       ("brainstorm" . ?b)
;;                       ("idea" . ?d)
;;                       ("current" . ?C)
;;                       ("work" . ?w)
;;                       ("baes" . ?B)
;;                       ("rrdl" . ?r)
;;                       ("offscreen" . ?O)
;;                       ("computer" .?c)
;;                       ("home" . ?h)
;;                       ("errand" . ?e)
;;                       ("emacs" . ?E)
;;                       ("orgmode" . ?o)
;;                       ("quicknote" . ?q)
;;                       ("joanna" . ?j)
;;                       ("harvey" . ?H)
;;                       ("sophie" . ?S)))

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
              (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
              (sequence "TODO(t)" "NEXT(n)" "DOING(D)" "PROJECT(p)"  "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(s@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "yellow")
              ("NEXT" :foreground "dark orange")
              ("PROJECT" :foreground "blue")
              ("DOING" :foreground "orchid")
              ("DONE" :foreground "ForestGreen")
              ("WAITING" :foreground "black" :background "yellow" :weight bold)
              ("SOMEDAY" :foreground "dark violet")
              ("HOLD" :foreground "magenta")
              ("CANCELLED" :foreground "snow4"))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(defface org-agenda-radcal-highlight-face `((t :foreground "SpringGreen3"))
    "Face used to highlight radcal entries in agenda view.
https://emacs.stackexchange.com/questions/69564/changing-the-color-of-items-in-org-mode-agenda-depending-on-keyword-tag")

(defface org-agenda-radcal-coding-highlight-face `((t :foreground "DarkTurquoise"))
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
            (when (re-search-forward "radbox_alt" line-end t)
              (font-lock-prepend-text-property line-begin line-end 'face 'org-agenda-radcal-alt-highlight-face))
            (when (re-search-forward "radcal_coding" line-end t)
              (font-lock-prepend-text-property line-begin line-end 'face 'org-agenda-radcal-coding-highlight-face))
            (when (re-search-forward "radbox_coding" line-end t)
              (font-lock-prepend-text-property line-begin line-end 'face 'org-agenda-radcal-coding-highlight-face))
            (when (re-search-forward "radcal" line-end t)
              (font-lock-prepend-text-property line-begin line-end 'face 'org-agenda-radcal-highlight-face))
            (when (re-search-forward "radbox" line-end t)
              (font-lock-prepend-text-property line-begin line-end 'face 'org-agenda-radcal-highlight-face))))
        (forward-line 1)))))

(add-hook 'org-agenda-finalize-hook #'org-agenda-highlight-radcal-entries)
