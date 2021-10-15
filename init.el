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

