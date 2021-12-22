(use-package ace-window
  :bind
  ("M-o" . ace-window))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1)
  :custom
  (golden-ratio-extra-commands '(ace-window
                                 windmove-down
                                 windmove-left
                                 windmove-right
                                 windmove-up))
  (golden-ratio-adjust-factor .8)
  (golden-ratio-auto-scale t)
  (golden-ratio-exclude-modes '("ediff-mode"
                                "magit-mode"
                                "dired-mode"
                                "eshell-mode"))
  (golden-ratio-wide-adjust-factor .8))
