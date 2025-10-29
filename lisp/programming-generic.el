(setq treesit-language-source-alist
      '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
        (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
		(clojure . ("https://github.com/sogaiu/tree-sitter-clojure" "v0.0.13"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))

;; not bothered about these for now - throwing version mismatches
;; (setq treesit-language-source-alist
;;     (append
;;      '((gomod  . ("https://github.com/camdencheek/tree-sitter-go-mod"))
;;        (gowork . ("https://github.com/omertuc/tree-sitter-go-work")))
;;      treesit-language-source-alist))

(use-package treesit
  :ensure nil
  :hook
  ;; Use the modern *-ts-mode for languages that have it
  ((js-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (python-mode . python-ts-mode)
   (clojure-mode . clojure-ts-mode)))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :commands (copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)))
:config
(add-hook 'go-mode-hook
		  (lambda ()
            (setq-local indent-tabs-mode t
                        tab-width 8)))
(when (fboundp 'go-ts-mode)
  (add-hook 'go-ts-mode-hook
            (lambda ()
			  (setq-local indent-tabs-mode t
						  tab-width 8))))

(use-package simpc-mode
  :ensure nil
  :load-path "lisp/"
  :mode ("\\.h\\(pp\\)?\\'" . simpc-mode)
  :mode ("\\.c\\(pp\\)?\\'" . simpc-mode))

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

(use-package emmet-mode
  :ensure t
  :hook ((web-mode css-mode sgml-mode html-mode) . emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package eglot
  :ensure t
  :commands eglot-ensure
  :config
  (setq eglot-extend-to-xref t)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :inlayHintProvider)))

;; according to https://chatgpt.com/c/690268d4-4d1c-832e-9dcf-37950ba372c2  
(add-hook 'eglot-managed-mode-hook #'flymake-mode)

(with-eval-after-load 'eglot
  ;; Ensure Flycheck is out of the way if it happens to be on
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (bound-and-true-p flycheck-mode) (flycheck-mode -1))
              (flymake-mode 1))))

(use-package python
  :ensure nil ;; this is built in but we configure it here
  :mode ("\\.py\\'" . python-ts-mode)
  :config
  (setq-default indent-tabs-mode t)
		    (setq-default tab-width 4)
		    (setq-default py-indent-tabs-mode t)
  (setq python-indent-offset 4)
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . pyvenv-mode)
         (python-ts-mode . flycheck-mode))
  :bind (:map python-ts-mode-map
              ("C-c t p" . mrl/run-python-tests-for-project)
              ("C-c t a" . mrl/run-python-tests-for-app)
              ("C-c t b" . mrl/run-python-tests-in-buffer)
              ("C-c t f" . mrl/run-python-test-at-point)))

;;;; Go (Tree-sitter + Eglot + Company + Flymake)

;; Prefer go-ts-mode if your Emacs has it; otherwise use go-mode from MELPA.
(with-eval-after-load 'treesit
  (when (fboundp 'go-ts-mode)
    (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))))

;; Fallback major mode (if tree-sitter unavailable)
(use-package go-mode
  :ensure t
  :defer t)

;; Eglot ↔ gopls wiring and tuning
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((go-mode go-ts-mode) . ("gopls")))
  (setq-default eglot-workspace-configuration
                '((gopls
                   . ((ui.completion.usePlaceholders . t)
                      (gofumpt . t)               ; use gofumpt style
                      (staticcheck . t)           ; extra diagnostics
                      (analyses . ((unusedparams . t)
                                   (unreachable . t)))
                      (directoryFilters . ["-node_modules" "-.git"]))))))

(defun mrl/go--format+imports ()
  "Format buffer and organize imports via gopls (Eglot)."
  (when (eglot-current-server)
    (eglot-format-buffer)
    ;; Apply organizeImports to whole buffer; ignore if not available.
    (ignore-errors
      (eglot-code-actions (point-min) (point-max)
                          "source.organizeImports" t))))

(defun mrl/go--setup ()
  "Setup Go buffers: Eglot, Flymake, Company, save-hooks, indentation."
  (eglot-ensure)
  ;; Prefer Flymake (Eglot) not Flycheck.
  (when (bound-and-true-p flycheck-mode) (flycheck-mode -1))
  (flymake-mode 1)
  ;; Company as the completion UI (CAPF → Eglot).
  (setq-local company-backends '(company-capf))
  (company-mode 1)
  ;; Go indentation (what gofmt/gofumpt expect).
  (setq-local indent-tabs-mode t
              tab-width 8)
  ;; Format + organize imports on save (buffer-local).
  (add-hook 'before-save-hook #'mrl/go--format+imports nil t))

;; Hooks for both go-mode and go-ts-mode
(add-hook 'go-mode-hook #'mrl/go--setup)
(when (fboundp 'go-ts-mode)
  (add-hook 'go-ts-mode-hook #'mrl/go--setup))

;; --------
;; Simple compile/test helpers
;; --------

(defun mrl/go-project-root ()
  (or (when-let ((p (project-current))) (project-root p))
      (locate-dominating-file default-directory "go.mod")
      default-directory))

(defun mrl/go--compile-in (dir cmd)
  (let ((default-directory dir))
    (compile cmd)))

(defun mrl/go-build ()
  "go build in module root."
  (interactive)
  (mrl/go--compile-in (mrl/go-project-root) "go build ./..."))

(defun mrl/go-test-all ()
  "go test ./... in module root."
  (interactive)
  (mrl/go--compile-in (mrl/go-project-root) "go test ./..."))

(defun mrl/go-test-pkg ()
  "go test in the current buffer's directory (package)."
  (interactive)
  (let ((bufdir (file-name-directory (or (buffer-file-name) default-directory))))
    (mrl/go--compile-in bufdir "go test")))

(defun mrl/go-test-func ()
  "Run go test for the Test* function at point using -run."
  (interactive)
  (let* ((name (or
                (when (fboundp 'treesit-node-at)
                  (let* ((node (treesit-node-at (point)))
                         (def (and node (treesit-parent-until
                                         node (lambda (n)
                                                (member (treesit-node-type n)
                                                        '("function_declaration" "method_declaration"))))))
                         (nm (and def (treesit-node-text
                                       (or (treesit-node-child-by-field-name def "name")
                                           def)))))
                    nm))
                (thing-at-point 'symbol t))))
    (unless (and name (string-match-p "^Test" name))
      (user-error "Point is not inside a Test* function (got: %s)" (or name "nil")))
    (let ((bufdir (file-name-directory (or (buffer-file-name) default-directory))))
      (mrl/go--compile-in bufdir (format "go test -run '^%s$'" name)))))

;; Keybindings (same style as your Python helpers)
(with-eval-after-load 'go-mode
  (define-key go-mode-map   (kbd "C-c t p") #'mrl/go-test-pkg)
  (define-key go-mode-map   (kbd "C-c t a") #'mrl/go-test-all)
  (define-key go-mode-map   (kbd "C-c t f") #'mrl/go-test-func)
  (define-key go-mode-map   (kbd "C-c b")   #'mrl/go-build))

(when (fboundp 'go-ts-mode)
  (with-eval-after-load 'go-ts-mode
    (define-key go-ts-mode-map (kbd "C-c t p") #'mrl/go-test-pkg)
    (define-key go-ts-mode-map (kbd "C-c t a") #'mrl/go-test-all)
    (define-key go-ts-mode-map (kbd "C-c t f") #'mrl/go-test-func)
    (define-key go-ts-mode-map (kbd "C-c b")   #'mrl/go-build)))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (set-face-attribute 'font-lock-string-face nil :slant 'italic)))

(use-package pyvenv
:ensure t
:hook (python-ts-mode . (lambda ()
                          (let ((venv-dir (expand-file-name ".venv" (project-current))))
                            (when (file-directory-p venv-dir)
                              (pyvenv-activate venv-dir))))))

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

(use-package django-snippets
  :ensure t)

(use-package direnv
  :config
  (direnv-mode))

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(use-package dockerfile-mode)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'programming-generic)

(use-package shell-maker
  :ensure t)

;; (use-package acp
;;   :vc (:url "https://github.com/xenodium/acp.el"))

(add-to-list 'load-path "/home/lemon/.emacs.d/lisp/acp.el/")
(require 'acp)

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell"))

;; (setq agent-shell-google-authentication
;;       (agent-shell-google-make-authentication :login t))

(setq agent-shell-google-authentication
      (agent-shell-google-make-authentication
       :api-key (lambda () (auth-source-pass-get "gemini-key" "google_api_key"))))

;; With function
(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication
       :api-key (lambda () (auth-source-pass-get "api-key" "anthropic_api_key"))))


(setq agent-shell-openai-authentication
      (agent-shell-openai-make-authentication :login t))
