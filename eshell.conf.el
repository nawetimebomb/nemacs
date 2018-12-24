(setenv "PATH"
        (concat
         "/usr/local/bin:/usr/local/sbin:"
         (getenv "PATH")))

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")
            (add-to-list 'eshell-visual-commands "top")))

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil
      eshell-destroy-buffer-when-process-dies t)
