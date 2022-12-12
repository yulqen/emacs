(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("3d2e532b010eeb2f5e09c79f0b3a277bfc268ca91a59cdda7ffd056b868a03bc" default))
 '(display-line-numbers-type 'relative t)
 '(display-time-mode t)
 '(ispell-dictionary nil)
 '(notmuch-saved-searches
   '((:name "Inbox" :query "tag:inbox" :count-query "tag:inbox and tag:unread" :sort-order newest-first :key "i")
     (:name "Unread" :query "tag:unread" :sort-order newest-first :key "u")
     (:name "Sent" :query "tag:sent" :sort-order newest-first :key "s")
     (:name "All Mail" :query "*" :sort-order newest-first :key "a")
     (:name "School" :query "tag:school" :sort-order newest-first :key "S")
     (:name "Deleted" :query "tag:deleted" :sort-order newest-first :key "d")
     (:name "new" :query "tag:new")))
 '(package-selected-packages
   '(consult-notes ef-themes flycheck-clj-kondo pass vertico denote orderless consult-lsp undo-tree embark-consult embark marginalia markdown-mode eglot helm popup async notmuch ivy calfw-org calfw unicode-fonts deft cider org-roam org yasnippet-snippets which-key vterm use-package rainbow-delimiters pdf-tools paredit magit ledger-mode gruvbox-theme flycheck expand-region evil elpy elfeed-score diminish counsel cmake-mode browse-kill-ring beacon auto-package-update amx ace-window ace-jump-mode))
 '(safe-local-variable-values
   '((denote-known-keywords "defnucsyr" "metanote" "meeting" "cyber" "baes" "rrdl" "opberths" "swow")
     (org-hide-leading-stars \.t)
     (org-hide-macro-markers \.t)
     (denote-infer-keywords)
     (denote-known-keywords "defnucsyr" "meeting" "cyber" "baes" "rrdl" "opberths" "swow")
     (denote-known-keywords "food" "drink")))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Fixed" :foundry "UKWN" :slant normal :weight normal :height 132 :width normal)))))
