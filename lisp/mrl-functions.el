
;;; mrl-functions --- my stuff
;;; Commentary: bits and bobs
;;; Code:

(defun mrl/get-start-and-lines-in-region ()
  "Cumbersome newbie way to get start line and number of lines in region."
  (let (pos1 pos2)
    (setq pos1 (region-beginning)
          pos2 (region-end))
    (let ((line1 (line-number-at-pos pos1))
          (line2 (line-number-at-pos pos2)))
      (list line1 (- line2 line1)))))

(defun mrl/clear-check-region (start lines)
  "Clears check marks given a START line and number of LINES to run through."
  (setq count 0)
  (while (> lines 0)
    (save-excursion
      (goto-line (+ start count))
      (beginning-of-line)
      (forward-char 3)
      (delete-char 1)
      (insert-char ?\s))
    (next-line)
    (setq count (+ count 1))
    (setq lines (- lines 1))))


(defun mrl/clear-check-from-region ()
  "Interactive func to clear markdown checks from a region."
  (interactive)
  (save-excursion
    (mrl/clear-check-region
     (car (mrl/get-start-and-lines-in-region))
     (car (last (mrl/get-start-and-lines-in-region))))))

(defun mrl/clear-check-single-line (&optional lines)
  "Remove the check from LINES number of markdown checkbox lines."
  (interactive "p")
  (while (> lines 0)
    (save-excursion
      (beginning-of-line)
      (forward-char 3)
      (delete-char 1)
      (insert-char ?\s))
    (next-line)
    (setq lines (- lines 1))))

(defun mrl/timestamp ()
  "Insert a simple timestamp."
  (interactive)
  (let ((m  (nth 1 (decode-time) ))
        (h (nth 2 (decode-time))))
    (insert (concat (number-to-string h) ":" (number-to-string m) ": "))))

(defun mrl/increase-face-size (height)
  "Increases size of font to HEIGHT in points.
For example, 110 is good for laptops but maybe 180 for 4k."
  (interactive "New height: ")
  (set-face-attribute 'default nil :height height))

(defvar mrl/brackets '( "“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠")
  "A list of strings, each element is a string of 2 chars, the left bracket and a matching right bracket.
Used by `mrl/select-text-in-quote' and others.")

(defun mrl/select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes QUOTATION MARK, GRAVE ACCENT, and anything in variable `xah-brackets'.
This command ignores nesting. For example, if text is
「(a(b)c▮)」
the selected char is 「c」, not 「a(b)c」.

Thanks! URL `http://xahlee.info/emacs/emacs/emacs_select_quote_text.html'
Created: 2020-11-24
Version: 2023-11-14"
  (interactive)
  (let ((xskipChars (concat "^\"`" (mapconcat #'identity mrl/brackets ""))))
    (skip-chars-backward xskipChars)
    (push-mark (point) t t)
    (skip-chars-forward xskipChars)))

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

(defun mrl/misc-pick-font ()
  "Pick a font."
  (interactive)
  (let ((font-name (completing-read "Select font:"
                                    (font-family-list))))
    (if (member font-name (font-family-list))
        (set-face-attribute 'default nil :font font-name)
      (error "'%s' font not found" font-name))))

(defcustom mrl/python-test-runner 'django
  "The test runner to use for Python projects.
Can be set to \='dango or \='pytest.
This can be set per-project using file-local variables."
  :type '(choice (const :tag "Django Default" 'django)
                 (const :tag "Pytest" 'pytest))
  :group 'python)

(defun mrl/python-get-test-parts ()
  "Helper to get project root and test path components.
Returns a list: (PROJECT-ROOT RELATIVE-FILE-PATH MODULE-PATH)."
  (let* ((current-project (project-current))
         ;; First get the project object, then get its root string.
         (project-root (when current-project (project-root current-project)))
         (file-path (buffer-file-name)))
    (unless (and project-root file-path)
      (error "Not in a project or buffer is not visiting a file"))
    (list project-root
          (file-relative-name file-path project-root)
          (replace-regexp-in-string
           "/" "."
           (replace-regexp-in-string "\\.py\\'" "" (file-relative-name file-path project-root))))))

(defun mrl/python-run-test (test-target)
  "Execute a Python test command in the project root using the configured runner."
  (let* ((current-project (project-current))
         ;; Correctly get the project root as a string
         (project-root (when current-project (project-root current-project)))
         (command (cond
                   ((eq mrl/python-test-runner 'pytest)
                    (concat "pytest " test-target))
                   ((eq mrl/python-test-runner 'django)
                    (concat "python manage.py test " test-target))
                   (t (error "Unknown test runner: %s" mrl/python-test-runner)))))
    ;; Now project-root is a string path, so the rest of the function works
    (when project-root
      (let ((venv-dir (expand-file-name ".venv" project-root)))
        (when (and (fboundp 'pyvenv-activate) (file-directory-p venv-dir))
          (pyvenv-activate venv-dir)))
      (let ((default-directory project-root))
        (message "Running: %s" command)
        (compile command)))))

(defun mrl/run-python-test-at-point ()
  "Run the test function/method at point using the configured runner."
  (interactive)
  (let* ((parts (mrl/python-get-test-parts))
         (relative-path (cadr parts))
         (module-path (caddr parts))
         (node (treesit-node-at (point)))
         (defun-node (treesit-parent-until
                      node
                      (lambda (n) (member (treesit-node-type n) '("function_definition" "decorated_definition")))))
         (class-node (when defun-node
                       (treesit-parent-until
                        defun-node
                        (lambda (n) (equal (treesit-node-type n) "class_definition")))))
         (test-target nil))
    (unless defun-node
      (error "Not inside a function or method"))
    (let* ((func-name-node (or (treesit-node-child-by-field-name defun-node "name")
                               (when (equal (treesit-node-type defun-node) "decorated_definition")
                                 (treesit-node-child-by-field-name (treesit-node-child-by-field-name defun-node "definition") "name"))))
           (func-name (treesit-node-text func-name-node))
           (class-name (when class-node
                         (treesit-node-text (treesit-node-child-by-field-name class-node "name")))))
      (setq test-target
            (cond
             ((eq mrl/python-test-runner 'pytest)
              (if class-name
                  (concat relative-path "::" class-name "::" func-name)
                (concat relative-path "::" func-name)))
             ((eq mrl/python-test-runner 'django)
              (if class-name
                  (concat module-path "." class-name "." func-name)
                module-path)) ;; Fallback for function-based tests
             (t (error "Unknown test runner: %s" mrl/python-test-runner)))))
    (mrl/python-run-test test-target)))

(defun mrl/run-python-tests-in-buffer ()
  "Run all tests in the current buffer's file."
  (interactive)
  (let* ((parts (mrl/python-get-test-parts))
         (relative-path (cadr parts))
         (module-path (caddr parts))
         (test-target (if (eq mrl/python-test-runner 'pytest) relative-path module-path)))
    (mrl/python-run-test test-target)))

(defun mrl/run-python-tests-for-app ()
  "Run tests for the current file's app."
  (interactive)
  (let* ((parts (mrl/python-get-test-parts))
         (relative-path (cadr parts))
         (app-path (car (split-string relative-path "/")))
         (test-target (if (eq mrl/python-test-runner 'pytest) app-path app-path)))
    (if (and app-path (> (length app-path) 0))
        (mrl/python-run-test test-target)
      (error "Could not determine app from file path"))))

(defun mrl/run-python-tests-for-project ()
  "Run the entire test suite for the project."
  (interactive)
  (mrl/python-run-test ""))

(provide 'mrl-functions)
;;; mrl-functions.el ends here
