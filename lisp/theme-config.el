;; (use-package ef-themes
;;   :ensure t)

(use-package ef-themes
  :ensure t
  :init
  ;; This makes the Modus commands listed below consider only the Ef
  ;; themes.  For an alternative that includes Modus and all
  ;; derivative themes (like Ef), enable the
  ;; `modus-themes-include-derivatives-mode' instead.  The manual of
  ;; the Ef themes has a section that explains all the possibilities:
  ;;
  ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
  ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'ef-frost))

;; (setq ef-themes-headings ; read the manual's entry or the doc string
;;     '((0 variable-pitch light 1.9)
;;       (1 variable-pitch light 1.3)
;;       (2 variable-pitch regular 1.2)
;;       (3 variable-pitch regular 1.1)
;;       (4 variable-pitch regular 1.0)
;;       (5 variable-pitch 1.0) ; absence of weight means `bold'
;;       (6 variable-pitch 1.0)
;;       (7 variable-pitch 1.0)
;;       (t variable-pitch 1.0)))

;; (setq ef-themes-mixed-fonts nil
;;     ef-themes-variable-pitch-ui nil)

(mapc #'disable-theme custom-enabled-themes)

;;(ef-themes-select 'ef-tritanopia-light)

;;(use-package gruber-darker-theme
;;  :ensure t)
;;(load-theme 'gruber-darker t)
;;(use-package borland-blue-theme
;;   :ensure t)
;; (use-package autumn-light-theme
;;   :ensure t)

(use-package doric-themes
  :ensure t
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-marble doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection))

;(doric-themes-select 'doric-plum)

(provide 'theme-config)
