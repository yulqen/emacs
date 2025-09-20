(use-package cider
:ensure t
:hook ((cider-repl-mode . paredit-mode)
       (clojure-mode . eglot-ensure)) ; Added from your clojure-mode hook
:config

(setq cider-jack-in-default 'clojure-cli)
(setq nrepl-use-ssh-fallback-for-remote-hosts t)
(setq cidr-repl-clear-help-banner t)
(setq nrepl-hide-special-buffers t)
(setq cider-font-lock-dynamically nil)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-use-pretty-printing t)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-repl-display-help-banner nil)
)

(use-package clojure-mode
:ensure t
:hook ((clojure-mode . eglot-ensure)
       (clojure-mode . paredit-mode))
:config
(set-face-attribute 'clojure-ts-keyword-face nil :slant 'italic))

(use-package flycheck-clj-kondo
:ensure t
:hook (clojure-mode . flycheck-mode))

(use-package parseedn)

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
