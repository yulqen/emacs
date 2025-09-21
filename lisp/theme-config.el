(use-package ef-themes
  :ensure t)

(setq ef-themes-to-toggle '(ef-summer ef-winter))

(setq ef-themes-headings ; read the manual's entry or the doc string
    '((0 variable-pitch light 1.9)
      (1 variable-pitch light 1.3)
      (2 variable-pitch regular 1.2)
      (3 variable-pitch regular 1.1)
      (4 variable-pitch regular 1.0)
      (5 variable-pitch 1.0) ; absence of weight means `bold'
      (6 variable-pitch 1.0)
      (7 variable-pitch 1.0)
      (t variable-pitch 1.0)))

(setq ef-themes-mixed-fonts nil
    ef-themes-variable-pitch-ui nil)

(mapc #'disable-theme custom-enabled-themes)

(ef-themes-select 'ef-duo-dark)

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
