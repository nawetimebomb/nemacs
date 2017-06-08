;; spaceline configuration file

(use-package spaceline
  :ensure t
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode 1)
  (spaceline-emacs-theme)

  (defface spaceline/status
    '((t :background "#0000ff"
         :foreground "black"
         ))
    "Status-driven face color"
    :group 'spaceline-config)

  (if (eq system-type 'darwin)
      (setq-default
       powerline-height 32
       powerline-default-separator 'alternate)
    (setq-default
     powerline-height 24
     powerline-default-separator 'slant))

  (setq-default
   spaceline-separator-dir-left '(right . right)
   spaceline-separator-dir-right '(left . left))

  (spaceline-install
    'main
    '((buffer-modified :face region)
      (major-mode :face spaceline/status)
      (projectile-root :face powerline-active2)
      ((buffer-id which-function) :separator " @ " :face powerline-active1)
      (anzu :when active :face spaceline-modified))
    '((selection-info :face region :when mark-active)
      (version-control)
      (global :when active)
      (line-column)
      (buffer-position))))

(provide 'spaceline.config)
