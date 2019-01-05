(use-package magit
  :bind ("C-x g" . magit-status)
  :custom-face
  (magit-section-heading ((t (:foreground "ForestGreen"
                                          :weight bold))))
  (magit-section-highlight ((t (:inherit hl-line))))
  (magit-diff-file-heading ((t (:inherit default))))
  (magit-diff-file-heading-highlight ((t (:inherit hl-line))))
  (git-commit-summary ((t (:inherit default)))))
