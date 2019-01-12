(use-package powerline
  :config
  (powerline-nano-theme)
  :custom-face
  (mode-line ((t (:background "DarkSlateBlue"
                              :box nil
                              :foreground "white"))))
  (mode-line-buffer-id ((t (:foreground "white"
                                        :weight bold))))
  (mode-line-emphasis ((t (:foreground "white"
                                       :weight bold))))
  (mode-line-highlight ((t (:foreground "yellow"))))
  (mode-line-inactive ((t (:background "gray95"
                                       :box (:color "gray30")
                                       :foreground "gray30")))))
