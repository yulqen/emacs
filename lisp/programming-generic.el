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
