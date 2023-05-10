(use-package rjsx-mode
  :preface
  (defun nemacs-setup-rjsx-mode ()
    (flycheck-mode)
    (lsp-mode))
  :hook
  (rjsx-mode . nemacs-setup-rjsx-mode)
  :mode "\\.js\\'"
  :custom
  (js-indent-level 4)
  (sgml-basic-offset 4))
