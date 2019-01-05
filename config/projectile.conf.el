(use-package projectile
  :delight
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-cache-file (expand-file-name "projectile-cache" nemacs-cache-dir))
  (projectile-enable-caching t)
  (projectile-globally-ignored-directories '(".git" "node_modules" ".local" ".svn"))
  (projectile-globally-ignored-file-suffixes '(".png"))
  (projectile-globally-ignored-files '(".gitignore" "TAGS"))
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks" nemacs-cache-dir))
  (projectile-mode-line '(:eval (if (not (equal (projectile-project-name) "-")) (concat " ["(projectile-project-name)"]") "")))
  :init (projectile-global-mode))
