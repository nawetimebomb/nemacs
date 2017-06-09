;; spaceline configuration file

(use-package spaceline
  :ensure t
  :config

  (spaceline-define-segment elnawe/version-control
    "Show the current version control branch."
    (when vc-mode
      (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name))))))))

(use-package spaceline-config
  :ensure nil
  :after spaceline
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))
                spaceline-separator-dir-left '(right . right)
                spaceline-separator-dir-right '(left . left)
                spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (spaceline-emacs-theme)
  (spaceline-helm-mode 1)

  (if (eq system-type 'darwin)
      (setq-default
       powerline-height 32
       powerline-default-separator 'alternate)
    (setq-default
     powerline-height 24
     powerline-default-separator 'slant))

  (spaceline-install
    '((major-mode :face highlight-face)
      (projectile-root :face powerline-active2)
      ((buffer-id which-function) :separator " @ " :face powerline-active1)
      (anzu :when active :face spaceline-modified))
    '((selection-info :face region :when mark-active)
      (elnawe/version-control)
      (global :when active)
      (line-column)
      (buffer-position :face highlight-face)))

  (spaceline-install
    'helm
    '((helm-buffer-id :face spaceline-read-only)
      (helm-number)
      (helm-prefix-argument))
    '((global :face region)))

  (set-face-attribute 'spaceline-modified nil
                      :background colors/red
                      :foreground colors/black)

  (set-face-attribute 'spaceline-unmodified nil
                      :background colors/green
                      :foreground colors/black)

  (set-face-attribute 'spaceline-read-only nil
                      :background colors/blue
                      :foreground colors/black))

(provide 'spaceline.config)
