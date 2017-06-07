;; git-gutter configuration file

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  (git-gutter:linum-setup)
  (custom-set-variables
   '(git-gutter:window-width 1)
   '(git-gutter:added-sign " ")
   '(git-gutter:deleted-sign " ")
   '(git-gutter:modified-sign " ")
   '(git-gutter:update-interval 0))
  (set-face-background 'git-gutter:modified custom-git-modified-color)
  (set-face-background 'git-gutter:added custom-git-added-color)
  (set-face-background 'git-gutter:deleted custom-git-deleted-color))

(provide 'git-gutter.config)
