(use-package marginalia
  :init
  (marginalia-mode))

(use-package project
  :custom
  (project-list-file (expand-file-name "projects" nemacs-cache-dir)))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (vertico-cycle t))
