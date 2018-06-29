;; Interaction between helm and projectile
(helm-projectile-on)

(setq projectile-cache-file (expand-file-name "projectile-cache" nemacs-cache-dir)
      projectile-enable-caching t
      projectile-keymap-prefix (kbd "C-c p")
      projectile-globally-ignored-directories '(".git" "node_modules")
      projectile-globally-ignored-file-suffixes '(".")
      projectile-known-projects-file (expand-file-name "projectile-bookmarks" nemacs-cache-dir)
      projectile-mode-line '(:eval (if (not (equal (projectile-project-name) "-")) (concat " ["(projectile-project-name)"]") ""))
      projectile-switch-project-action 'helm-projectile)

(global-set-key [remap projectile-find-file] #'helm-projectile)
(global-set-key [remap helm-projectile-find-file] #'helm-projectile)
