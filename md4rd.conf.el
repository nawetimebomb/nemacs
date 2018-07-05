(setq md4rd-subs-active '(argentina
                          commandline
                          emacs
                          linuxmasterrace
                          lisp+Common_Lisp
                          leagueoflegends
                          unixporn))

(add-hook 'md4rd-mode-hook #'hl-line-mode)

(define-key md4rd-mode-map (kbd "n") 'widget-forward)
(define-key md4rd-mode-map (kbd "p") 'widget-backward)
