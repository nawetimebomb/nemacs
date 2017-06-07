;; multiple-cursors configuration file

(use-package multiple-cursors
  :config
  (multiple-cursors-mode t)
  :bind
  ("C-S-l" . mc/mark-all-like-this))

(provide 'multiple-cursors.config)
