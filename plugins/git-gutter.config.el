;; git-gutter configuration file

(use-package git-gutter
  :ensure t
  :config
  (require 'git-gutter)
  (global-git-gutter-mode 1)
  (git-gutter:linum-setup))

(provide 'git-gutter.config)
