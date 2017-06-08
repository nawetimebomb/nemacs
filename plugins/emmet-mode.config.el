;; emmet's support for emacs by @smihica.
;; https://github.com/smihica/emmet-mode
;; configuration file by @elnawe.

(use-package emmet-mode
  :ensure t
  :config
  (emmet-mode t)
  (setq emmet-indentation 4)
  (setq emmet-self-closing-tag-style " /")
  (setq emmet-indent-after-insert nil)
  (setq emmet-expand-jsx-className? t))

(provide 'emmet-mode.config)
