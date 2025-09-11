(use-package ob-clojurescript
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (clojure . t)
   (clojurescript . t)
   (emacs-lisp . t)
   (shell . t)
   (js . t)
   (perl . t)
   (css . t)
   (makefile . t)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)

(setq org-babel-clojure-backend 'cider)

(setq org-babel-python-command "/usr/bin/python3")

(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)

(provide 'org-babel-config)
