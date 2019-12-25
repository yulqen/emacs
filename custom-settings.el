(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auto-revert-verbose nil)
 '(company-begin-commands (quote (self-insert-command)))
 '(company-global-modes (quote (not shell-mode eaf-mode)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-require-match (quote never))
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes
   (quote
    ("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "947190b4f17f78c39b0ab1ea95b1e6097cc9202d55c73a702395fc817f899393" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default)))
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(elfeed-feeds
   (quote
    ("https://www.youtube.com/feeds/videos.xml?channel_id=UChWbNrHQHvKK6paclLp7WYw" "https://www.reddit.com/r/linuxmasterrace.rss" "https://feeds.feedburner.com/arstechnica/open-source" "https://www.computerweekly.com/rss/IT-security.xml" "https://www.fsf.org/static/fsforg/rss/news.xml" "https://www.reddit.com/r/freebsd.rss" "https://www.reddit.com/r/emacs.rss" "https://www.reddit.com/r/rugbyunion/.rss" "http://pragmaticemacs.com/feed/" "https://200ok.ch/atom.xml" "https://www.youtube.com/feeds/videos.xml?channel_id=UCkK9UDm_ZNrq_rIXCz3xCGA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCFzGyNKXPAglNq28qWYTDFA" "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" "http://www.linuxinsider.com/perl/syndication/rssfull.pl" "http://planet.debian.org/rss20.xml" "http://feeds2.feedburner.com/Command-line-fu" "https://opensource.org/news.xml" "https://feeds.feedburner.com/arstechnica/index" "https://www.wired.com/feed/rss" "https://sivers.org/en.atom")))
 '(fci-rule-color "#383838")
 '(global-auto-revert-non-file-buffers t)
 '(helm-completion-style (quote emacs))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(ivy-count-format "【%d/%d】")
 '(ivy-height 10)
 '(ivy-magic-slash-non-match-action (quote ivy-magic-slash-non-match-create))
 '(ivy-on-del-error-function nil)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(lsp-ui-doc-border "wheat")
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-sort-agenda-notime-is-late nil)
 '(package-selected-packages
   (quote
    (lsp-python-ms ace-window discover-my-major popup-kill-ring amx gnu-elpa-keyring-update auto-package-update monokai-theme color-theme-monokai color-theme-sanityinc-tomorrow dracula-theme evil-mode yasnippet-snippets highlight-symbol rainbow-delimiters git-gutter Ivy org-caldav which-key-mode org-plus-contrib use-package ac-cider magit blacken py-autopep8 flycheck elpy yasnippet company-lsp company lsp-ui go-autocomplete lsp-mode go-mode markdown-mode gruvbox-theme helm org cider slime evil)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#282828")))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(recentf-auto-cleanup "05:00am")
 '(recentf-exclude
   (quote
    ((expand-file-name package-user-dir)
     ".cache" ".cask" ".elfeed" "bookmarks" "cache" "ido.*" "persp-confs" "recentf" "undo-tree-hist" "url" "COMMIT_EDITMSG\\'")))
 '(recentf-mode t)
 '(setq "05:00am" t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(winner-boring-buffers
   (quote
    ("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*" "*esh command on file*"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))
