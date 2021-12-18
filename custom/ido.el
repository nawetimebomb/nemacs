(use-package ido
  :ensure flx-ido
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  :custom
  (ido-save-directory-list-file (expand-file-name "ido.last" nemacs-cache-dir))
  (ido-enable-prefix nil)
  (ido-enable-flex-matching t)
  (ido-case-fold nil)
  (ido-auto-merge-work-directories-length -1)
  (ido-create-new-buffer 'always)
  (ido-use-filename-at-point nil)
  (ido-max-prospects 10)
  (ido-use-faces nil))
