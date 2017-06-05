;; git-gutter configuration file

(use-package git-gutter
  :config
  (global-git-gutter-mode 1)
  (git-gutter:linum-setup)
  (custom-set-variables
   '(git-gutter:added-sign " ")
   '(git-gutter:deleted-sign " ")
   '(git-gutter:modified-sign " ")))

(provide 'git-gutter.config)
