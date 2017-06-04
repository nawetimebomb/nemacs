;; Helm configuration file

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)
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
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-b" . helm-buffers-list))

(provide 'helm.config)
