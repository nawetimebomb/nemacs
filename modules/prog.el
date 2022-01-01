(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package log-edit
  :custom
  (log-edit-confirm 'changed)
  (log-edit-keep-buffer nil)
  (log-edit-require-final-newline t))

(use-package lsp-mode)
