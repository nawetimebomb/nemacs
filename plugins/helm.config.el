;; Helm configuration file

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq-default
   helm-always-two-windows t
   helm-display-header-line nil)
  (add-to-list 'helm-boring-buffer-regexp-list "\\*")
  (helm-autoresize-mode t)
  (set-face-attribute 'helm-selection nil
                      :background custom-background-menu-selection-color
                      :foreground custom-foreground-menu-selection-color
                      :bold t)
  (set-face-attribute 'helm-source-header nil
                      :background custom-background-menu-header-color
                      :foreground custom-foreground-menu-header-color
                      :height 1.8
                      :box nil)
  (set-face-attribute 'helm-action nil :underline nil)
  (set-face-attribute 'helm-match nil :background nil)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-b" . helm-buffers-list))

(use-package helm-buffers
  :ensure nil
  :after helm
  :config
  (setq-default
   helm-buffers-fuzzy-matching t
   helm-buffer-max-length nil)
  (set-face-attribute 'helm-buffer-directory nil :inherit 'dired-directory)
  (set-face-attribute 'helm-non-file-buffer nil :inherit 'shadow))

(use-package helm-projectile
  :ensure t
  :after helm
  :config (helm-projectile-toggle 1))

(provide 'helm.config)
