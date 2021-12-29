(use-package rjsx-mode
  :mode "\\.js\\'"
  :hook
  (rjsx-mode . nemacs-setup-default-prog-mode)
  :custom
  (js-indent-level 4)
  (sgml-basic-offset 4)
  :custom-face
  (js2-error ((t :underline (:style wave :color "#3e9688")))))
