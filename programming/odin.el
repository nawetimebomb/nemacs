(use-package odin-mode
  :straight (odin-mode
             :type git
             :host github
             :repo "mattt-b/odin-mode")
  :preface
  (defun nemacs-setup-odin-mode ()
    (flycheck-mode))
  :hook
  (odin-mode . nemacs-setup-odin-mode))
