(use-package ace-window
  :bind
  ("M-o" . ace-window))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1)

  (when nemacs-widescreen
    (call-interactively 'golden-ratio-toggle-widescreen))
  :custom
  (golden-ratio-extra-commands '(ace-window
                                 windmove-down
                                 windmove-left
                                 windmove-right
                                 windmove-up))
  (golden-ratio-adjust-factor 1)
  (golden-ratio-auto-scale nil)
  (golden-ratio-exclude-modes '("ediff-mode"
                                "magit-mode"
                                "dired-mode"
                                "eshell-mode"))
  (golden-ratio-wide-adjust-factor 1))

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-popup-type 'minibuffer)
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 0.2)
  (which-key-idle-secondary-delay nil))
