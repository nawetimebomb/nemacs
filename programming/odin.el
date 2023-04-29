(use-package odin-mode
  :straight (odin-mode
             :type git
             :host github
             :repo "mattt-b/odin-mode")
  :hook
  (odin-mode . lsp))
