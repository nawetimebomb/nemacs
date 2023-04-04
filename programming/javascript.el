(use-package rjsx-mode
  :preface
  (defun nemacs-setup-rjsx-mode ()
    (flycheck-mode))
  :mode "\\.js\\'"
  :hook
  (rjsx-mode . nemacs-setup-rjsx-mode)
  :custom
  (js-indent-level 4)
  (sgml-basic-offset 4))
