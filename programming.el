(use-package company-mode)

(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package cc-mode
  :preface
  (defun nemacs-setup-cc-mode ()
    (setq flycheck-clang-language-standard "gnu99")
    (flycheck-mode)
    (c-set-offset 'case-label 4)
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

(use-package go-mode
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'case-label '+))))

(use-package rjsx-mode
  :preface
  (defun nemacs-setup-rjsx-mode ()
    (flycheck-mode))
  :hook
  (rjsx-mode . nemacs-setup-rjsx-mode)
  :mode "\\.js\\'"
  :custom
  (js-indent-level 4)
  (sgml-basic-offset 4))

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

(add-hook
 'prog-mode-hook #'(lambda ()
                     (company-mode)
                     (display-line-numbers-mode)))
