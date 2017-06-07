;; Projectile: is a project interaction library for Emacs by @bbatsov.
;; https://github.com/bbatsov/projectile
;; configuration file by @elnawe.

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  :bind
  ("C-p" . projectile-command-map))

(provide 'projectile.config)
