(use-package astro-zombies-theme
  :disabled t
  :straight (:host github :repo "elnawe/astro-zombies-theme")
  :init
  (load-theme 'astro-zombies t))

(use-package naysayer-theme
  :straight (naysayer-theme :type git :host github :repo "nickav/naysayer-theme.el"
                            :fork (:host codeberg
                                   :repo "nawetimebomb/naysayer-theme.el"))
  :init
  (load-theme 'naysayer t))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-color-icons nil))

(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode))

(use-package doom-modeline
  :disabled t
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-bar-width 0)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-height 35)
  (doom-modeline-major-mode-color-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-mu4e t))
