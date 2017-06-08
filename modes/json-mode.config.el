;; Major mode for editing JSON files with emacs by @joshwnj
;; https://github.com/joshwnj/json-mode
;; configuration file by @elnawe.

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :interpreter "json")

(provide 'json-mode.config)
