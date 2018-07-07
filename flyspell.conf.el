(when (executable-find "hunspell")
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US"
        ispell-local-dictionary "en_US"
        ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

(define-key flyspell-mode-map (kbd "M-t") #'nil)
