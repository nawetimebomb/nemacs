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

(provide 'major-modes.config)
