;; Helm configuration file

(use-package helm
  :ensure t
  :config
  (setq-default
   helm-always-two-windows t
   helm-display-header-line nil)
  (set-face-attribute 'helm-selection nil
                      :background helm/selection--background
                      :foreground helm/selection--foreground
                      :bold t)
  (set-face-attribute 'helm-source-header nil
                      :background helm/source-header--background
                      :box nil
                      :foreground helm/source-header--foreground
                      :height helm/source-header--height)
  (set-face-attribute 'helm-action nil
                      :underline nil)
  (set-face-attribute 'helm-match nil
                      :background nil))

(use-package helm-config
  :ensure helm
  :config
  (helm-mode t)
  (helm-autoresize-mode t)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-b" . helm-buffers-list))

(use-package helm-buffers
  :ensure helm
  :after helm
  :config
  (add-to-list 'helm-boring-buffer-regexp-list "\\*")
  (setq-default
   helm-buffers-fuzzy-matching t
   helm-buffer-max-length nil)
  (set-face-attribute 'helm-buffer-directory nil :inherit 'dired-directory)
  (set-face-attribute 'helm-non-file-buffer nil :inherit 'shadow))

(use-package helm-projectile
  :ensure t
  :after helm
  :config
  (helm-projectile-toggle 1))

(provide 'helm.config)
