(use-package consult
  :bind
  (("M-g g" . consult-goto-line)
   ("M-y" . consult-yank-from-kill-ring)
   ("C-x b" . consult-buffer)
   ("C-x B" . consult-buffer-other-window)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

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
