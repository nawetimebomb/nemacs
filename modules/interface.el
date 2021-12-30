(use-package base16-theme
  :init
  (load-theme 'base16-apathy t)
  :custom-face
  (font-lock-comment-face ((t :foreground "#9ba3a2")))
  (font-lock-constant-face ((t :foreground "DarkTurquoise")))
  (font-lock-negation-char-face ((t :foreground "red3" :weight bold)))
  (fringe ((t :background nil)))
  (mode-line-inactive ((t (:background "#0a312a" :foreground "#2b685e"))))
  (success ((t :foreground "#b266c0" :weight bold))))

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-bar-width 2)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-height 35)
  (doom-modeline-major-mode-color-icon nil)
  (doom-modeline-minor-modes nil)
  :custom-face
  (doom-modeline-bar ((t :background nil)))
  (doom-modeline-buffer-file ((t :inherit doom-modeline-buffer-major-mode)))
  (doom-modeline-buffer-modified ((t :inherit doom-modeline-evil-insert-state)))
  (doom-modeline-info ((t :weight normal))))
