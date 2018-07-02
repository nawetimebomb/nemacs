(add-to-list 'custom-theme-load-path (concat user-emacs-directory "/themes"))

(setq zenburn-override-colors-alist
      '(("zenburn-bg-2" . "#171717")))
(load-theme 'zenburn t)
