;; anzu configuration file

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode)
  (custom-set-variables
   '(anzu-cons-mode-line-p nil))
  (custom-set-faces
   '(anzu-mode-line ((t (:foreground "#1d1f21"))))
   '(anzu-mode-line-no-match ((t (:foreground "#1d1f21"))))
   '(anzu-replace-highlight ((t nil)))))

(provide 'anzu.config)
