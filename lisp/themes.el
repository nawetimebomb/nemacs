;; |==============================================|
;; |  title: themes.el                            |
;; |  description: themes configuration           |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t)
  (if (eq system-type 'darwin)
      (setq ns-use-srgb-colorspace t)
    (setq ns-use-srgb-colorspace t)))

(provide 'themes)
