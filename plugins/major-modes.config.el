;; major-modes configuration file

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "javascript")

(use-package json-mode
  :mode "\\.json\\'"
  :interpreter "json")

(use-package web-mode
  :mode "\\.html\\'"
  :interpreter "html")

(use-package org-mode
  :mode "\\.org\\'"
  :interpreter "org")

(use-package csharp-mode
  :mode "\\.cs\\'"
  :interpreter "csharp")

(use-package emacs-lisp-mode
  :mode "\\.el\\'"
  :interpreter "emacs-lisp")

(use-package rjsx-mode
  :mode "components\\/.*\\.js\\'"
  :interpreter "rjsx"
  :config
  (setq tab-width 4))

;; Fix these two!
(setq js2-mode-hook
  '(lambda () (progn
                (set-variable 'indent-tabs-mode nil)
                (set-variable 'tab-width 4))))
(setq rjsx-mode
  '(lambda () (progn
    (set-variable 'indent-tabs-mode nil))))

(provide 'major-modes.config)
