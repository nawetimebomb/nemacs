(use-package company
  :init
  (global-company-mode t))

(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package log-edit
  :custom
  (log-edit-confirm 'changed)
  (log-edit-keep-buffer nil)
  (log-edit-require-final-newline t))

(use-package lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(odin-mode . "odin"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "/usr/bin/ols")
                    :major-modes '(odin-mode)
                    :server-id 'ols
                    :multi-root t))
  :custom
  (lsp-auto-guess-root t)
  (lsp-keymap-prefix "s-f"))
