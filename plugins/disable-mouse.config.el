;; Disable the mouse in Emacs by @purcell.
;; https://github.com/purcell/disable-mouse
;; configuration file by @elnawe.

(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode)
  :bind
  ("ESC M-m" . global-disable-mouse-mode))

(provide 'disable-mouse.config)
