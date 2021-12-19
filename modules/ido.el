(use-package ido
  :ensure t
  :init
  (ido-mode 1)
  :custom
  (ido-save-directory-list-file (expand-file-name "ido.last" nemacs-cache-dir))
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-use-filename-at-point 'guess)
  (ido-create-new-buffer 'always)
  (ido-file-extensions-order '(".org" ".js" ".jsx" ".scss"))
  (ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "node_modules"))
  (ido-ignore-buffers '("\\` " "\\*")))
