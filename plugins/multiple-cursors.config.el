;; multiple-cursors configuration file

(use-package multiple-cursors
  :config
  (require 'multiple-cursors)
  :bind
  ("C-S-l" . mc/mark-all-like-this))

(provide 'multiple-cursors.config)
