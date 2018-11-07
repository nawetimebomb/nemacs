(add-to-list 'custom-theme-load-path (concat user-emacs-directory "/themes"))

(setq zenburn-override-colors-alist
      '(("zenburn-bg-2" . "#171717")
        ("zenburn-fg-1" . "#A9A9A9")))
(load-theme 'zenburn t)

(zenburn-with-color-variables
  (custom-set-faces
   `(button ((t (:underline nil))))
   `(font-lock-warning-face ((t (:foreground ,zenburn-green :bold t))))
   `(mode-line ((t (:foreground ,zenburn-fg))))))
