;; packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			                   ("org" . "https://orgmode.org/elpa/")
			                   ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; start of use-package
(require 'use-package)


(require 'org)
(add-to-list 'org-modules 'org-habit)
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" default))
 '(ledger-binary-path "hledger")
 '(org-agenda-files
   '("~/org/home.org" "~/org/projects.org" "~/org/journal.org" "~/org/work.org" "~/org/notes.org" "~/org/habits.org" "~/org/calendar/cal.org"))
 '(package-selected-packages
   '(elfeed-score yasnippet-snippets yasnippet browse-kill-ring expand-region ace-window amx flycheck ace-jump-mode gruvbox-theme company helm auto-package-update ledger-mode magit elfeed-org which-key use-package rainbow-delimiters paredit evil counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
