(use-package ido
  :ensure t
  :init
  (ido-mode 1)
  :config
  (defadvice ido-switch-buffer (around no-confirmation activate)
    (let ((confirm-nonexistent-file-or-buffer nil)) ad-do-it))

  (defadvice ido-find-file (around no-confirmation activate)
    (let ((confirm-nonexistent-file-or-buffer nil)) ad-do-it))
  :custom
  (ido-save-directory-list-file (expand-file-name "ido.last" nemacs-cache-dir))
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-use-filename-at-point 'guess)
  (ido-create-new-buffer 'always)
  (ido-file-extensions-order '(".org" ".js" ".jsx" ".scss"))
  (ido-ignore-files '("\\`CVS/"
                      "\\`#"
                      "\\`.#"
                      "\\`\\.\\./"
                      "\\`\\./"
                      "node_modules"
                      "__OLD__"))
  (ido-ignore-buffers '("\\` " "\\*")))
