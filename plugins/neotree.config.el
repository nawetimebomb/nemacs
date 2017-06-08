;; A emacs tree plugin like NerdTree for Vim by @jaypei.
;; https://github.com/jaypei/emacs-neotree
;; configuration file by @elnawe.

(use-package neotree
  :ensure t
  :config
  (setq-default neo-smart-open t)
  (setq neo-theme 'classic)
  (setq neo-vc-integration '(face char))
  (setq neo-toggle-window-keep-p t)
  (set-face-attribute 'neo-vc-edited-face nil
                      :foreground custom-git-modified-color)
  (set-face-attribute 'neo-vc-added-face nil
                      :foreground custom-git-added-color)
  :bind
  ([f8] . neotree-toggle))

(provide 'neotree.config)
