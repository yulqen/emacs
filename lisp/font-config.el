(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 180)
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :height 1.0)
(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 1.0)

(setq fontaine-latest-state-file
    (locate-user-emacs-file "fontaine-latest-state.eld"))

(setq fontaine-presets
    '((small
       :default-family "DejaVu Sans Mono"
       :default-height 120
       :variable-pitch-family "DejaVu Sans Mono")
      (regular) ; like this it uses all the fallback values and is named `regular'
      (medium
       :default-weight semilight
       :default-height 130
       :bold-weight extrabold)
      (large
       :inherit medium
       :default-height 150)
      (presentation
       :default-height 180)
      (t
       ;; I keep all properties for didactic purposes, but most can be
       ;; omitted.  See the fontaine manual for the technicalities:
       ;; <https://protesilaos.com/emacs/fontaine>.
       :default-family "DejaVu Sans Mono"
       ;; :default-family "Aporetic Sans Mono"
       :default-weight regular
       :default-height 100

       :fixed-pitch-family nil ; falls back to :default-family
       :fixed-pitch-weight nil ; falls back to :default-weight
       :fixed-pitch-height 1.0

       :fixed-pitch-serif-family nil ; falls back to :default-family
       :fixed-pitch-serif-weight nil ; falls back to :default-weight
       :fixed-pitch-serif-height 1.0

       :variable-pitch-family "Aporetic Serif"
       :variable-pitch-weight nil
       :variable-pitch-height 1.0

       :mode-line-active-family nil ; falls back to :default-family
       :mode-line-active-weight nil ; falls back to :default-weight
       :mode-line-active-height 0.9

       :mode-line-inactive-family nil ; falls back to :default-family
       :mode-line-inactive-weight nil ; falls back to :default-weight
       :mode-line-inactive-height 0.9

       :header-line-family nil ; falls back to :default-family
       :header-line-weight nil ; falls back to :default-weight
       :header-line-height 0.9

       :line-number-family nil ; falls back to :default-family
       :line-number-weight nil ; falls back to :default-weight
       :line-number-height 0.9

       :tab-bar-family nil ; falls back to :default-family
       :tab-bar-weight nil ; falls back to :default-weight
       :tab-bar-height 1.0

       :tab-line-family nil ; falls back to :default-family
       :tab-line-weight nil ; falls back to :default-weight
       :tab-line-height 1.0

       :bold-family nil ; use whatever the underlying face has
       :bold-weight bold

       :italic-family nil
       :italic-slant italic

       :line-spacing nil)))

;; Set the last preset or fall back to desired style from `fontaine-presets'
;; (the `regular' in this case).
;(fontaine-set-preset (or (fontaine-restore-latest-preset) 'medium))

;; Persist the latest font preset when closing/starting Emacs and
;; while switching between themes.
;(fontaine-mode 1)

;; fontaine does not define any key bindings.  This is just a sample that
;; respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
;(define-key global-map (kbd "C-c f") #'fontaine-set-preset)

(provide 'font-config)
