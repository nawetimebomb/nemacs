(use-package spaceline
  :ensure t
  :init
  (progn
    (require 'spaceline-config)
    (spaceline-emacs-theme)
    (setq powerline-default-separator 'chamfer
          powerline-height 19
          spaceline-highlight-face-func 'spaceline-highlight-face-modified
          spaceline-separator-dir-left '(left . left)
          spaceline-separator-dir-right '(right . right))

    (spaceline-define-segment elnawe::buffer-state-and-id
      "Shows the buffer state (modified, saved, etc) and the buffer id"
      (concat
       "%l:%c "
       (if buffer-read-only
           "= "
         (if (buffer-modified-p)
             "* "
           "- "))
       (buffer-name)))

    (spaceline-define-segment elnawe::time
      (format-time-string "%H:%M" (current-time)))

    (spaceline-install
      '((elnawe::buffer-state-and-id :face highlight-face)
        (projectile-root :face powerline-active1)
        (selection-info :when mark-active :face powerline-active1)
        (anzu :when active :face powerline-active1))
      '((hud buffer-position :face powerline-active1)
        (elnawe::time :face powerline-active1)
        (major-mode :face highlight-face)))))

(provide 'my-spaceline-config)
