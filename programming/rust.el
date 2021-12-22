(use-package rustic)

(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-mode . nemacs-setup-default-prog-mode))
