;; Helm configuration file

(require 'helm-config)

(helm-mode t)

(set-face-attribute 'helm-selection nil
                    :background "#ffb269"
                    :foreground "#151515")
(set-face-attribute 'helm-source-header nil
                    :background nil
                    :foreground "#ffffff"
                    :height 1.8
                    :box nil)
(helm-autoresize-mode t)
(add-to-list 'helm-boring-buffer-regexp-list "\\*")

(provide 'helm.config)
