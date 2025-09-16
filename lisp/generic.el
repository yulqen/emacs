(setq idle-update-delay 1.0)
(add-hook 'focus-out-hook #'garbage-collect)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq-default cursor-in-non-selected-windows nil)
(setq hightlight-nonselected-windows nil)
(setq x-stretch-cursor t)
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

(setq dired-recursive-copies 'always)

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode)

(setq make-backup-files nil          ; don't create backup files
      create-lockfiles nil           ; don't create lockfiles
      auto-save-default nil          ; don't auto-save to #file#
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))) ; but if auto-save is on, put it in /tmp

(display-time-mode t)

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

(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

(setq calendar-latitude 55.77)
(setq calendar-longitude -2.01)
(setq calendar-location-name "Berwick-upon-Tweed")

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

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

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

(use-package vertico
  :ensure t
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :custom
  (context-menu-mode t)
  (completion-cycle-threshold 3) ;; see corfu config
  (tab-always-indent 'complete) ;; see corfu config
  (text-mode-ispell-word-completion nil) ;; see corfu config
  (read-extended-command-predicate #'command-completion-default-include-p)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package ansi-color
  :hook ((compilation-filter . ansi-color-compilation-filter)
         (shell-mode-hook . ansi-color-for-comint-mode)))
(add-hook 'async-shell-command-after-hook 'ansi-color-for-comint-mode)

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-show-minibuffer-help t)
  (setq undo-tree-minibuffer-help-dynamic t))

(use-package which-key
:ensure t
:config
(which-key-mode))

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

(setq browse-url-browser-function 'eww-browse-url)

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

(use-package beacon
:ensure t
:hook (prog-mode . beacon-mode))

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
)

(use-package ibuffer :ensure nil
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-title-face 'font-lock-doc-face)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups
        '(("Main"
           ("Directories" (mode . dired-mode))
           ("C++" (or
                   (mode . c++-mode)
                   (mode . c++-ts-mode)
                   (mode . c-mode)
                   (mode . c-ts-mode)
                   (mode . c-or-c++-ts-mode)))
           ("Python" (or
                      (mode . python-ts-mode)
                      (mode . c-mode)
                      (mode . python-mode)))
           ("Build" (or
                     (mode . make-mode)
                     (mode . makefile-gmake-mode)
                     (name . "^Makefile$")
                     (mode . change-log-mode)))
           ("Scripts" (or
                       (mode . shell-script-mode)
                       (mode . shell-mode)
                       (mode . sh-mode)
                       (mode . lua-mode)
                       (mode . bat-mode)))
           ("Config" (or
                      (mode . conf-mode)
                      (mode . conf-toml-mode)
                      (mode . toml-ts-mode)
                      (mode . conf-windows-mode)
                      (name . "^\\.clangd$")
                      (name . "^\\.gitignore$")
                      (name . "^Doxyfile$")
                      (name . "^config\\.toml$")
                      (mode . yaml-mode)))
           ("Web" (or
                   (mode . mhtml-mode)
                   (mode . html-mode)
                   (mode . web-mode)
                   (mode . nxml-mode)))
           ("CSS" (or
                   (mode . css-mode)
                   (mode . sass-mode)))
           ("JS" (or
                  (mode . js-mode)
                  (mode . rjsx-mode)))
           ("Markup" (or
                   (mode . markdown-mode)
                   (mode . adoc-mode)))
           ("Org" (mode . org-mode))
           ("LaTeX" (name . "\.tex$"))
           ("Magit" (or
                     (mode . magit-blame-mode)
                     (mode . magit-cherry-mode)
                     (mode . magit-diff-mode)
                     (mode . magit-log-mode)
                     (mode . magit-process-mode)
                     (mode . magit-status-mode)))
           ("Apps" (or
                    (mode . elfeed-search-mode)
                    (mode . elfeed-show-mode)))
           ("Fundamental" (or
                           (mode . fundamental-mode)
                           (mode . text-mode)))
           ("Emacs" (or
                     (mode . emacs-lisp-mode)
                     (name . "^\\*Help\\*$")
                     (name . "^\\*Custom.*")
                     (name . "^\\*Org Agenda\\*$")
                     (name . "^\\*info\\*$")
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Backtrace\\*$")
                     (name . "^\\*Messages\\*$"))))))
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "Main")))
)
(global-set-key [(f5)]  #'ibuffer)

(provide 'generic)
