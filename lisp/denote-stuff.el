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

(provide 'denote-stuff)
