;; spaceline configuration file

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-hud-on)
  (spaceline-toggle-version-control-on)
  (spaceline-toggle-line-column-on))

(provide 'spaceline.config)
