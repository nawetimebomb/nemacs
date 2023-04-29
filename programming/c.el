(use-package cc-mode
  :preface
  (defun nemacs-setup-cc-mode ()
    (setq flycheck-clang-language-standard "gnu99")
    (c-set-offset 'case-label 4)
    (lsp-mode)
    (c-toggle-comment-style -1))

  (defun nemacs-compile-sources ()
    (interactive)
    (async-shell-command (concat (project-root (project-current)) "build.sh -norun") nil nil))
  :hook
  (c-mode . nemacs-setup-cc-mode)
  :bind
  ("<f10>"  . nemacs-compile-sources)
  :custom
  (c-basic-offset 4))
