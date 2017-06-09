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
   '(git-gutter:hide-gutter nil)
   '(git-gutter:update-interval 0))
  (zenburn-with-color-variables
    (set-face-attribute 'git-gutter:added nil
                        :background zenburn-green)
    (set-face-attribute 'git-gutter:deleted nil
                        :background zenburn-red)
    (set-face-attribute 'git-gutter:modified nil
                        :background zenburn-blue)))

(provide 'git-gutter.config)
