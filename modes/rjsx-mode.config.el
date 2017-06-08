;; A JSX major mode for Emacs by @felipeochoa
;; https://github.com/felipeochoa/rjsx-mode
;; configuration file by @elnawe.

(use-package rjsx-mode
  :ensure t
  :mode "components/.*\\.js\\'"
  :interpreter "jsx"
  :config
  (set-variable tab-width 4))

(provide 'rjsx-mode.config)
