;; dumb-jump configuration file

(use-package dumb-jump
  :ensure t
  :after helm
  :config
  (dumb-jump-mode 1)
  (setq dumb-jump-selector 'helm)
  :bind
  ([f12] . dumb-jump-go)
  ("C-M-S-g" . dumb-jump-go-prefer-external-other-window))

(provide 'dumb-jump.config)
