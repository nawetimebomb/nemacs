;; Improved JavaScript editing mode for GNU Emacs by @mooz.
;; https://github.com/mooz/js2-mode
;; configuration file by @elnawe.

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "javascript"
  :config
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  :bind
  ("C-c m j" . js2-mode))

(provide 'js2-mode.config)
