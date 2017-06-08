;; It's Magit! A Git Porcelain inside Emacs by @magit.
;; https://github.com/magit/magit
;; configuration file by @elnawe.

(use-package magit
  :ensure t
  :bind
  ("C-c g s" . magit-status))

(provide 'magit.config)
