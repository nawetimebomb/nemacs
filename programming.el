(use-package cc-mode
  :preface
  (defun nemacs-setup-cc-mode ()
    (c-set-offset 'case-label 4)
    (c-toggle-comment-style -1)
    (setq-local compile-command "cd .. && make -k "))
  :hook
  (c-mode . nemacs-setup-cc-mode)
  :bind
  ("<f10>"  . recompile)
  :custom
  (c-basic-offset 4))

(use-package go-mode
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'case-label '+))))

(use-package rjsx-mode
  :preface
  (defun nemacs-setup-rjsx-mode ())
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
    (setq-local compile-command "cd .. && make -k "))
  :bind
  ("<f10>"  . recompile)
  :hook
  (odin-mode . nemacs-setup-odin-mode))

(add-hook
 'prog-mode-hook #'(lambda ()
                     (display-line-numbers-mode)))

(use-package v-mode
  :straight (v-mode
             :type git
             :host github
             :repo "damon-kwok/v-mode"
             :files ("tokens" "v-mode.el"))
  :config
  :bind-keymap
  ("M-z" . v-menu)
  ("<f6>" . v-menu)
  ("C-c C-f" . v-format-buffer)
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode))


(when (file-exists-p "~/repos/stanczyk")
  (add-to-list 'load-path "~/repos/stanczyk/")
  (require 'stanczyk-mode))

(global-set-key (kbd "C-M-<down>") #'duplicate-line)
