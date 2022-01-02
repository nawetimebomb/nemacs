;;; EXWM
;; NOTE: This configuration looks different from the others, everything is inside `use-package:config'.
;;       The reason for that is that EXWM is very sensible on how we initialize and change variables and
;;       most of then need to be initialized before running `(exwm-enable)'. Hopefully, this is the only
;;       configuration that is not following "the rule" (although there's no rule imposed, really).

(use-package exwm
  :config
  (use-package exwm-edit
    :config
    (exwm-edit-mode))

  (use-package exwm-systemtray
    :straight nil
    :config
    (exwm-systemtray-enable))

  ;; Enable system modes
  (display-time-mode)
  (display-battery-mode)

  ;; Map CapsLock to CTRL
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/os/Xmodmap")

  (add-hook 'exwm-update-title-hook #'nemacs-exwm-rename-buffer)
  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)

  (setq exwm-workspace-number 3)

  (setq exwm-input-global-keys
        `(([?\s-&]                . nemacs-exwm-run-application)
          ([?\s-r]                . exwm-reset)
          ([?\s-w]                . exwm-workspace-switch)

          ([s-left]               . windmove-left)
          ([s-down]               . windmove-down)
          ([s-up]                 . windmove-up)
          ([s-right]              . windmove-right)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-enable))
