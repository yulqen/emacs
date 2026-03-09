(use-package tmr
  :ensure t
  :config
  (define-key global-map (kbd "C-c t") #'tmr-prefix-map)
  (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/camera-shutter.oga"
        tmr-notification-urgency 'normal
        tmr-description-list 'tmr-description-history))
