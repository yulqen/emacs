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

(set-face-attribute 'default nil :height 140)

;; use org mode as scratch buffer
(setq initial-major-mode 'org-mode)

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(require 'use-package)
(setq use-package-always-ensure t)

(defun mrl/org-word-count ()
  "Count words in region/buffer, estimate pages, and reading time.
Excludes lines beginning with * or #. Prints result in echo area. 
Ripped from : https://chrismaiorana.com/summer-productivity-reset-emacs-functions/"
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (word-count
          (save-excursion
            (goto-char start)
            (let ((count 0)
                  (inhibit-field-text-motion t))
              (while (< (point) end)
                (beginning-of-line)
                (unless (looking-at-p "^[*#<]")
                  (let ((line-end (line-end-position)))
                    (while (re-search-forward "\\w+\\W*" line-end t)
                      (setq count (1+ count)))))
                (forward-line 1))
              count)))
         (words-per-page 400)
         (reading-speed 215)
         (page-count (/ (+ word-count words-per-page -1) words-per-page))
         (reading-time (/ (+ word-count reading-speed -1) reading-speed)))
    (message "%d words, ~%d pages, ~%d min read"
             word-count page-count reading-time)))

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

(use-package gruber-darker-theme)
(use-package borland-blue-theme)
(use-package autumn-light-theme)

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

;; (use-package org-roam
;;    :ensure t
;;    :custom
;;    (org-roam-dailies-directory "daily/")
;;    (org-roam-directory "~/Documents/org")
;;    (org-roam-capture-ref-templates
;;     '(("h" "default" plain
;;        "%?"
;;        :target (file+head "home/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;        :unnarrowed t)))
;;    (org-roam-capture-templates
;;     '(("h" "default" plain
;;        "%?"
;;        :target (file+head "home/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;        :unnarrowed t)
;;       ("w" "work" plain
;;        "%?"
;;        :target (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;        :unnarrowed t)
;;       ("c" "mod+cpr" plain
;;        "%?"
;;        :target (file+head "work/cpr/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;        :unnarrowed t)
;;       ("e" "encrypted" plain
;;        "%?"
;;        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
;;        :unnarrowed t)))
;;    (org-roam-dailies-capture-templates
;;     '(("h" "home" entry "* %<%T>: %?"
;;        :target (file+head "home/%<%Y-%m-%d>.org" "#+title: %<%A %Y-%m-%d>\n")
;;        :unnarrowed t)
;;       ("w" "work" entry "* %<%T>: %?"
;;        :target (file+head "work/%<%Y-%m-%d>.org" "#+title: %<%A %Y-%m-%d>\n")
;;        :unnarrowed t)))
;;    :bind (("C-c n l" . org-roam-buffer-toggle)
;;           ("C-c n f" . org-roam-node-find)
;;           ("C-c n i" . org-roam-node-insert)
;;           ("C-c n n" . org-roam-dailies-capture-today)
;;           ("C-c n t" . org-roam-dailies-goto-today)
;;           :map org-roam-mode-map
;;           ("y" . org-roam-dailies-goto-previous-note)
;;           ("t" . org-roam-dailies-goto-next-note)
;;           ("d" . org-roam-dailies-goto-date)
;;           ("D" . org-roam-dailies-capture-date))
;;    :bind-keymap ("C-c n D" . org-roam-mode-map)
;;    :config
;;    ;; this should allow us to type spaces in ido buffer when creating new nodes
;;    ;; from https://org-roam.discourse.group/t/org-roam-node-find-space-not-allowed-in-node-title/1847/6
;;    (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)
;;    (defun mrl/search-roam ()
;;      "Run consult-ripgrep on the org roam directory"
;;      (interactive)
;;      (consult-ripgrep org-roam-directory nil))
;;    (require 'org-roam-protocol)
;;    (org-roam-db-autosync-mode)
;;    ;; Bind this to C-c n I
;;    (defun org-roam-node-insert-immediate (arg &rest args)
;;      (interactive "P")
;;      (let (([[id:06d0a643-662b-4440-9e1a-9b9dcf6e2dcb][test_node]]args (cons arg args))
;;            (org-roam-capture-templates (list (append (car org-roam-capture-templates)
;;                                                      '(:immediate-finish t)))))
;;        (apply #'org-roam-node-insert args)))
;;    :bind (("C-c n I" . org-roam-node-insert-immediate)))

;; (use-package consult-org-roam
;;    :ensure t
;;    :after org-roam
;;    :init
;;    (require 'consult-org-roam)
;;    ;; Activate the minor mode
;;    (consult-org-roam-mode 1)
;;    :custom
;;    ;; Use `ripgrep' for searching with `consult-org-roam-search'
;;    (consult-org-roam-grep-func #'consult-ripgrep)
;;    ;; Configure a custom narrow key for `consult-buffer'
;;    (consult-org-roam-buffer-narrow-key ?r)
;;    ;; Display org-roam buffers right after non-org-roam buffers
;;    ;; in consult-buffer (and not down at the bottom)
;;    (consult-org-roam-buffer-after-buffers t)
;;    :config
;;    ;; Eventually suppress previewing for certain functions
;;    (consult-customize
;;     consult-org-roam-forward-links
;;     :preview-key "M-.")
;;    :bind
;;    ;; Define some convenient keybindings as an addition
;;    ("C-c n e" . consult-org-roam-file-find)
;;    ("C-c n b" . consult-org-roam-backlinks)
;;    ("C-c n B" . consult-org-roam-backlinks-recursive)
;;    ("C-c n l" . consult-org-roam-forward-links)
;;    ("C-c n r" . consult-org-roam-search))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind ;; C-c bindings (mode-specific-map)
  ("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c k" . consult-kmacro)
  ;; C-x bindings (ctl-x-map)
  ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
  ;;("C-x b" . consult-buffer) ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
  ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
  ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
  ;; Custom M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop)     ;; orig. yank-pop
  ("<help> a" . consult-apropos) ;; orig. apropos-command
  ;; M-g bindings (goto-map)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
  ("M-g g" . consult-goto-line)   ;; orig. goto-line
  ("M-g M-g" . consult-goto-line) ;; orig. goto-line
  ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings (search-map)
  ;; ("M-s d" . consult-find)
  ;; ("M-s D" . consult-locate)
  ;; ("M-s g" . consult-grep)
  ;; ("M-g G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ;; ("M-s l" . consult-line)
  ;; ("M-s L" . consult-line-multi)
  ;; ("M-s m" . consult-multi-occur)
  ;; ("M-s k" . consult-keep-lines)
  ;; ("M-s u" . consult-focus-lines)
  ;; ;; Isearch integration
  ;; ("M-s e" . consult-isearch-history)
  ;;:map isearch-mode-map
  ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
  ;; Minibuffer history
  ;;:map minibuffer-local-map
  ("M-s" . consult-history) ;; orig. next-matching-history-element
  ("M-r" . consult-history) ;; orig. previous-matching-history-element
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
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))
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

;; OLD DENOTE CONFIG - COMMENTED JULY25
;; (use-package denote
;;   :ensure t
;;   :init
;;   (add-hook 'dired-mode-hook #'denote-dired-mode)
;;   :config
;;   (setq denote-directory (expand-file-name "~/Documents/denote/"))
;;   (setq denote-known-keywords '("emacs" "clojure" "org-mode" "work" "technote"))
;;   (setq denote-file-type 'text)
;;   (setq denote-prompts '(title keywords))
;;   (setq denote-date-prompt-use-org-read-date t)

;;   (defun mrl/denote-find-file ()
;;       "Find file in the current `denote-directory'."
;;       (interactive)
;;       (require 'consult)
;;       (require 'denote)
;;       (consult-find (denote-directory)))

;;   (defun mrl/is-todays-journal? (f)
;;     "If f is today's journal in denote, f is returned"
;;     (let* ((month-regexp (car (calendar-current-date)))
;;            (day-regexp (nth 1 (calendar-current-date)))
;;            (year-regexp (nth 2 (calendar-current-date)))
;;            (journal-files (directory-files (denote-directory) nil "_journal"))
;;            (day-match? (string-match-p (concat "^......" (format "%02d" day-regexp)) f))
;;            (year-match? (string-match-p (concat "^" (number-to-string year-regexp)) f))
;;            (month-match? (string-match-p (concat (number-to-string month-regexp) "..T") f)))
;;       (when (and day-match? year-match? month-match?)
;;         f)))

;;   (defvar my-denote-silo-directories
;;   `("/home/lemon/Documents/mod-denote"
;;     ;; You don't actually need to include the `denote-directory' here
;;     ;; if you use the regular commands in their global context.  I am
;;     ;; including it for completeness.
;;     ,denote-directory)
;;   "List of file paths pointing to my Denote silos.
;;   This is a list of strings.")

;; (defvar my-denote-commands-for-silos
;;   '(denote
;;     denote-date
;;     denote-subdirectory
;;     denote-template
;;     denote-type)
;;   "List of Denote commands to call after selecting a silo.
;;   This is a list of symbols that specify the note-creating
;;   interactive functions that Denote provides.")

;; (defun my-denote-pick-silo-then-command (silo command)
;;   "Select SILO and run Denote COMMAND in it.
;;   SILO is a file path from `my-denote-silo-directories', while
;;   COMMAND is one among `my-denote-commands-for-silos'."
;;   (interactive
;;    (list (completing-read "Select a silo: " my-denote-silo-directories nil t)
;;          (intern (completing-read
;;                   "Run command in silo: "
;;                   my-denote-commands-for-silos nil t))))
;;   (let ((denote-directory silo))
;;     (call-interactively command)))

;;   (defun mrl/denote-journal ()
;;     "Create an entry tagged journal with the date as its title."
;;     (interactive)
;;     (defvar mrl/in-mod-denote nil)
;;     (let* ((journal-dir (concat (denote-directory) "journals"))
;;            (today-journal
;;             (car (-non-nil
;;                   (mapcar #'mrl/is-todays-journal? (directory-files journal-dir nil "_journal"))))))
;;       (if today-journal
;;           (find-file (concat journal-dir "/" today-journal))
;;         (if mrl/in-mod-denote ; this variable is from the .dir-locals.el file in the silo directory; we want to use a specific template
;;             (denote
;;              (format-time-string "%A %e %B %Y")
;;              '("journal") nil journal-dir nil 'modjournal)
;;           (denote
;;            (format-time-string "%A %e %B %Y")
;;            '("journal") nil journal-dir)))))
  
;;   :bind (("C-c n n" . denote-create-note)
;;          ("C-c n d" . mrl/denote-journal)
;;          ("C-c n t" . denote-type)
;;          ("C-c n f" . mrl/denote-find-file)
;;          ("C-c n l" . denote-link))
;;   )

(use-package denote
  :ensure t
  :hook
  (;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/Documents/denote/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("emacs" "computer" "family" "health"))
  (setq denote-infer-keywords t)
  ;;(setq denote-file-type 'text)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-history-completion-in-prompts t)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

(use-package denote-silo
  :ensure t
  ;; Bind these commands to key bindings of your choice.
  :commands ( denote-silo-create-note
              denote-silo-open-or-create
              denote-silo-select-silo-then-command
              denote-silo-dired
              denote-silo-cd )
  :bind
  ( :map global-map
    ("C-c n s" . denote-silo-open-or-create)
    ("C-c n S" . denote-silo-select-silo-then-command))
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories
        (list denote-directory
              "~/Documents/denote/"
              "~/Documents/dft-denote/")))

(use-package denote-journal
  :ensure t
  ;; Bind those to some key for your convenience.
  :commands ( denote-journal-new-entry
              denote-journal-new-or-existing-entry
              denote-journal-link-or-create-entry )
  :hook (calendar-mode . denote-journal-calendar-mode)
  :bind
  ( :map global-map
    ("C-c n j". denote-journal-new-or-existing-entry))
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year))

(require 'fontaine)

(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))

;; Aporetic is my highly customised build of Iosevka:
;; <https://github.com/protesilaos/aporetic>.
(setq fontaine-presets
      '((small
         :default-family "Aporetic Serif Mono"
         :default-height 80
         :variable-pitch-family "Aporetic Sans")
        (regular) ; like this it uses all the fallback values and is named `regular'
        (medium
         :default-weight semilight
         :default-height 115
         :bold-weight extrabold)
        (large
         :inherit medium
         :default-height 150)
        (presentation
         :default-height 180)
        (t
         ;; I keep all properties for didactic purposes, but most can be
         ;; omitted.  See the fontaine manual for the technicalities:
         ;; <https://protesilaos.com/emacs/fontaine>.
         :default-family "Aporetic Sans Mono"
         :default-weight regular
         :default-height 100

         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0

         :fixed-pitch-serif-family nil ; falls back to :default-family
         :fixed-pitch-serif-weight nil ; falls back to :default-weight
         :fixed-pitch-serif-height 1.0

         :variable-pitch-family "Aporetic Serif"
         :variable-pitch-weight nil
         :variable-pitch-height 1.0

         :mode-line-active-family nil ; falls back to :default-family
         :mode-line-active-weight nil ; falls back to :default-weight
         :mode-line-active-height 0.9

         :mode-line-inactive-family nil ; falls back to :default-family
         :mode-line-inactive-weight nil ; falls back to :default-weight
         :mode-line-inactive-height 0.9

         :header-line-family nil ; falls back to :default-family
         :header-line-weight nil ; falls back to :default-weight
         :header-line-height 0.9

         :line-number-family nil ; falls back to :default-family
         :line-number-weight nil ; falls back to :default-weight
         :line-number-height 0.9

         :tab-bar-family nil ; falls back to :default-family
         :tab-bar-weight nil ; falls back to :default-weight
         :tab-bar-height 1.0

         :tab-line-family nil ; falls back to :default-family
         :tab-line-weight nil ; falls back to :default-weight
         :tab-line-height 1.0

         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold

         :italic-family nil
         :italic-slant italic

         :line-spacing nil)))

;; Set the last preset or fall back to desired style from `fontaine-presets'
;; (the `regular' in this case).
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; Persist the latest font preset when closing/starting Emacs and
;; while switching between themes.
(fontaine-mode 1)

;; fontaine does not define any key bindings.  This is just a sample that
;; respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
(define-key global-map (kbd "C-c f") #'fontaine-set-preset)

;; ef-themes configuration
;; Make customisations that affect Emacs faces BEFORE loading a theme
;; (any change needs a theme re-load to take effect).
(use-package ef-themes
  :ensure t)

;; If you like two specific themes and want to switch between them, you
;; can specify them in `ef-themes-to-toggle' and then invoke the command
;; `ef-themes-toggle'.  All the themes are included in the variable
;; `ef-themes-collection'.
(setq ef-themes-to-toggle '(ef-summer ef-winter))

(setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 variable-pitch light 1.9)
        (1 variable-pitch light 1.3)
        (2 variable-pitch regular 1.2)
        (3 variable-pitch regular 1.1)
        (4 variable-pitch regular 1.0)
        (5 variable-pitch 1.0) ; absence of weight means `bold'
        (6 variable-pitch 1.0)
        (7 variable-pitch 1.0)
        (t variable-pitch 1.0)))

;; They are nil by default...
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the theme of choice:
(load-theme 'ef-day :no-confirm)

;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
;; (ef-themes-select 'ef-deuteranopia-dark)

;; The themes we provide are recorded in the `ef-themes-dark-themes',
;; `ef-themes-light-themes'.

;; We also provide these commands, but do not assign them to any key:
;;
;; - `ef-themes-toggle'
;; - `ef-themes-select'
;; - `ef-themes-select-dark'
;; - `ef-themes-select-light'
;; - `ef-themes-load-random'
;; - `ef-themes-preview-colors'
;; - `ef-themes-preview-colors-current'

(use-package doric-themes
  :ensure t
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)

  ;;(doric-themes-select 'doric-plum)

  ;; ;; To load a random theme instead, use something like one of these:
  ;;
  ;; (doric-themes-load-random)
  ;; (doric-themes-load-random 'light)
  ;; (doric-themes-load-random 'dark)

  ;; ;; For optimal results, also define your preferred font family (or use my `fontaine' package):
  ;;
  ;; (set-face-attribute 'default nil :family "Aporetic Sans Mono" :height 160)
  ;; (set-face-attribute 'variable-pitch nil :family "Aporetic Sans" :height 1.0)
  ;; (set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono" :height 1.0)

  :bind
  (("<f5>" . doric-themes-toggle)
   ("C-<f5>" . doric-themes-select)
   ("M-<f5>" . doric-themes-rotate)))

(use-package flycheck-clj-kondo
  :hook (after-init . global-flycheck-mode))

(use-package magit)

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

;; (use-package mu4e
;;   :ensure nil
;;   :load-path "/usr/local/share/emacs/site-lisp/mu4e"
;;   :config
;;   (setq mail-user-agent 'mu4e-user-agent)
;;   (setq mu4e-sent-folder   "/Sent"
;;         mu4e-drafts-folder "/Drafts"
;;         mu4e-refile-folder "/Archive"
;;         mu4e-trash-folder  "/Trash")
;;   (setq mu4e-maildir-shortcuts
;;         '((:maildir "/Archive" :key ?a)
;;           (:maildir "/inbox"   :key ?i)
;;           (:maildir "/work"    :key ?w)
;;           (:maildir "/sent"    :key ?s)))
;;   (setq mu4e-headers-fields
;;         '((:date          .  25)
;;           (:flags         .   6)
;;           (:from          .  22)
;;           (:subject       .  nil)))
;;   (add-to-list 'mu4e-bookmarks
;;                '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t))
;;   (setq mu4e-get-mail-command "mbsync purelymailchannel")
;;   (setq mu4e-compose-reply-to-address "matt@matthewlemon.com"
;;         user-mail-address "matt@matthewlemon.com"
;;         user-full-name  "Matthew Lemon")
;;   (setq message-signature "M R Lemon\n")
;;   (setq message-send-mail-function 'smtpmail-send-it
;;         smtpmail-default-smtp-server "smtp.purelymail.com"
;;         smtpmail-smtp-user "mrlemon@purelymail.com"
;;         smtpmail-stream-type 'ssl
;;         smtpmail-smtp-service 465
;;         smtpmail-smtp-server "smtp.purelymail.com")
;;   (setq message-kill-buffer-on-exit t))

;; (require 'mu4e-transient)
;; (global-set-key (kbd "C-c m") #'mu4e-transient-menu)

(use-package cider
  :config
  (setq cider-jack-in-default 'clojure-cli)
  (setq nrepl-use-ssh-fallback-for-remote-hosts t))

(use-package clojure-mode)
(use-package parseedn)

(use-package undo-tree
  :config
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

;; (use-package paredit
;;   :hook ((clojure-mode . enable-paredit-mode)
;;          (cider-repl-mode . enable-paredit-mode)
;;          (emacs-lisp-mode . enable-paredit-mode)
;;          (eval-expression-minibuffer-setup . enable-paredit-mode)
;;          (ielm-mode . enable-paredit-mode)
;;          (lisp-mode . enable-paredit-mode)
;;          (lisp-interaction-mode . enable-paredit-mode)
;;          (scheme-mode . enable-paredit-mode))
;;   :config
;;   ;; Do this if you get problems
;;   ;;(define-key paredit-mode-map (kbd "RET") nil)
;;   (show-paren-mode t))

;; (use-package forge
;;   :after magit)

(use-package which-key
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package simpc-mode
  :ensure nil
  :load-path "lisp/"
  :mode ("\\.h\\(pp\\)?\\'" . simpc-mode)
  :mode ("\\.c\\(pp\\)?\\'" . simpc-mode))

;; (use-package smex
;;   :bind (("M-x" . smex)
;;          ("M-X" . smex-major-mode-commands)
;;          ("C-c C-c M-x" . execute-extended-command)))

;; (use-package ido-completing-read+
;;   :config
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (ido-ubiquitous-mode 1)
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-create-new-buffer 'always)
;;   (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".md" ".xml" ".el" ".ini"))
;;   (setq ido-enable-flex-matching t))

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
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package elfeed
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

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package pyvenv
  :hook (python-mode . (lambda ()
                        (let ((venv-dir (expand-file-name ".venv" (projectile-project-root))))
                          (when (file-directory-p venv-dir)
                            (pyvenv-activate venv-dir))))))

(use-package django-mode)
(use-package django-snippets)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("C-c y" . yas-expand)))

(use-package yasnippet-snippets)
(use-package clojure-snippets)

(use-package elpy
  :config
  (elpy-enable))

(use-package direnv
  :config
  (direnv-mode))

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(use-package beacon
  :config
  (beacon-mode 1))

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

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package diminish
  :config
  (diminish 'projectile-mode)
  (diminish 'completion-preview-mode)
  (diminish 'which-key-mode)
  (diminish 'beacon-mode))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-global-modes '(not org-mode)))

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

;; Enable LSP support by default in programming buffers
;;(add-hook 'prog-mode-hook #'eglot-ensure)

;;; Indication of local VCS changes
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))

;; Enable `diff-hl' support by default in programming buffers
(add-hook 'prog-mode-hook #'diff-hl-mode)

;; Enable autocompletion by default in programming buffers
;; (rc/require 'corfu)
;; (add-hook 'prog-mode-hook #'corfu-mode)

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
                               "~/Documents/org/radbox_alt.org"
                               "~/Documents/org/radcal_coding.org"
                               "~/Documents/org/radbox_work.org"
                               "~/Documents/org/alphabet_learning.org"
                               "~/Documents/org/dft.org"
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
          (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "TODO")
                                      (org-agenda-sorting-strategy '(alpha-up))))
          (tags-todo "TODO=\"WAITING\"" ((org-agenda-overriding-header "DfT WAITING")
                                         (org-agenda-sorting-strategy '(deadline-down scheduled-down priority-down))))
          (tags-todo "-SCHEDULED>=\"<today>\"&TODO=\"NEXT\""
                     ((org-agenda-overriding-header "DfT NEXT UNSCHEDULED")
                      (org-agenda-sorting-strategy '(deadline-up priority-down))))
          (tags-todo "TODO=\"PROJECT\"" ((org-agenda-overriding-header "Projects")
                                         (org-agenda-sorting-strategy '(alpha-up))))
          )
         ((org-agenda-category-filter-preset '("+DfT" "+Proj/Task" "+radcal" "+radcal_alt" "+radcal_work" "+radcal_coding" "+Meeting" "+WorkTrip" "+refile"))))

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
          (tags-todo "TODO=\"NEXT\"" ((org-agenda-overriding-header "All Next Actions")
                                      (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down))))
          (tags-todo "TODO=\"TODO\"" ((org-agenda-overriding-header "TODO")
                                      (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down))))
          (tags-todo "TODO=\"WAITING\"" ((org-agenda-overriding-header "Home WAITING")
                                         (org-agenda-sorting-strategy '(deadline-down scheduled-down priority-down))))
          (tags-todo "-SCHEDULED>=\"<today>\"&TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Home NEXT UNSCHEDULED")
                      (org-agenda-sorting-strategy '(alpha-up deadline-down scheduled-down priority-down))))
          (tags "idea" ((org-agenda-overriding-header "Ideas")
                        (org-agenda-sorting-strategy '(alpha-up))))
          (tags-todo "TODO=\"PROJECT\"" ((org-agenda-overriding-header "Projects")
                                         (org-agenda-sorting-strategy '(alpha-up)))))
         ((org-agenda-category-filter-preset '("+home" "+habits" "+radcal" "+radcal_alt" "radcal_work" "+radcal_coding" "+refile" "+Birthday"))))
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
               "* %U %?\n%i\n%a"
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
