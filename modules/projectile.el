(use-package projectile
  :ensure t
  :commands (projectile-command-map
             projectile-find-file)
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories ".local")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "__OLD__")
  :custom
  (projectile-cache-file (expand-file-name "projectile.cache" nemacs-cache-dir))
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" nemacs-cache-dir)))
