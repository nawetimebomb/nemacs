(use-package projectile
  :ensure t
  :commands (projectile-command-map
             projectile-find-file)
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom
  (projectile-globally-ignored-directories '(".local"
                                             "node_modules"
                                             "__OLD__"
                                             ".idea"
                                             ".vscode"
                                             ".ensime_cache"
                                             ".eunit"
                                             ".git"
                                             ".hg"
                                             ".fslckout"
                                             "_FOSSIL_"
                                             ".bzr"
                                             "_darcs"
                                             ".tox"
                                             ".svn"
                                             ".stack-work"
                                             ".ccls-cache"
                                             ".cache"
                                             ".clangd"))
  (projectile-cache-file
   (expand-file-name "projectile.cache" nemacs-cache-dir))
  (projectile-known-projects-file
   (expand-file-name "projectile-bookmarks.eld" nemacs-cache-dir)))
