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
 '(display-line-numbers-type 'relative)
 '(display-time-mode t)
 '(package-selected-packages
   '(deft cider org-roam org yasnippet-snippets which-key vterm use-package rainbow-delimiters pdf-tools paredit notmuch magit ledger-mode gruvbox-theme flycheck expand-region evil elpy elfeed-score diminish counsel cmake-mode browse-kill-ring beacon auto-package-update amx ace-window ace-jump-mode))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 120 :width normal)))))
