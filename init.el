(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; set custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; require separate config files
(require 'fontaine)
(require 'mrl-functions)
(require 'denote-stuff)
(require 'org-babel-config)
(require 'theme-config)
(require 'font-config)
(require 'lisp-language-config)
(require 'generic)
(require 'programming-generic)
(require 'org-core)
(require 'email-calendar-rss)
(require 'reformatting-text)
