;; dumb-jump configuration file

(use-package dumb-jump
  :ensure t
  :config
  (dumb-jump-mode 1)
  (setq dumb-jump-selector 'helm)
  :bind
  ("C-M-S-g" . dumb-jump-go-prefer-external-other-window))

(provide 'dumb-jump.config)
