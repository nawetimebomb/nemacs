;; Emacs lisp mode
;; configuration file by @elnawe.

(use-package emacs-lisp-mode
  :ensure nil
  :mode "\\.el\\'"
  :interpreter "emacs-lisp"
  :bind
  ("C-c m e" . emacs-lisp-mode))

(provide 'emacs-lisp-mode.config)
