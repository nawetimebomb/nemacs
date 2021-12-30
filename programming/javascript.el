(use-package rjsx-mode
  :preface
  (defun nemacs-setup-rjsx-mode ()
    (flycheck-mode))
  :mode "\\.js\\'"
  :hook
  (rjsx-mode . nemacs-setup-rjsx-mode)
  :custom
  (js-indent-level 4)
  (sgml-basic-offset 4)
  :custom-face
  (rjsx-text ((t :inherit default)))
  (js2-error ((t :underline (:style wave :color "#3e9688")))))
