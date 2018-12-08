;; Interaction between helm and projectile
(helm-projectile-on)

(setq projectile-cache-file (expand-file-name "projectile-cache" nemacs-cache-dir)
      projectile-enable-caching t
      projectile-globally-ignored-directories '(".git" "node_modules" ".local" ".svn")
      projectile-globally-ignored-file-suffixes '(".png")
      projectile-globally-ignored-files '(".gitignore" "TAGS")
      projectile-known-projects-file (expand-file-name "projectile-bookmarks" nemacs-cache-dir)
      projectile-mode-line '(:eval (if (not (equal (projectile-project-name) "-")) (concat " ["(projectile-project-name)"]") ""))
      projectile-switch-project-action 'helm-projectile)

(define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
(global-set-key [remap projectile-find-file] #'helm-projectile)
(global-set-key [remap helm-projectile-find-file] #'helm-projectile)
