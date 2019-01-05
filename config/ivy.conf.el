(use-package ivy
  :bind (("C-x B" . ivy-switch-buffer-other-window))
  :delight
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :init (ivy-mode 1))

(use-package counsel
  :after ivy
  :delight
  :init (counsel-mode))

(use-package counsel-projectile
  :after counsel
  :custom-face
  (ivy-virtual ((t (:foreground "gray50" :inherit default))))
  :init (require 'subr-x)
  :config (counsel-projectile-mode))
