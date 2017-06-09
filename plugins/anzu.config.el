;; anzu configuration file

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode)
  (custom-set-variables
   '(anzu-cons-mode-line-p nil))
  (custom-set-faces
   '(anzu-replace-highlight ((t nil)))))

(provide 'anzu.config)
