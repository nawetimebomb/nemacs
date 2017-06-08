;; A major-mode for editing C# in emacs by @josteink.
;; https://github.com/josteink/csharp-mode
;; configuration file by @elnawe.

(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'"
  :interpreter "csharp")

(provide 'csharp-mode.config)
