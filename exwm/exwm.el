(use-package exwm
  :config
  (use-package exwm-edit
    :config
    (exwm-edit-mode))

  (use-package exwm-systemtray
    :straight nil
    :config
    (exwm-systemtray-enable))

  ;; Map CapsLock to CTRL
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  (defun nemacs-exwm-run-application (command)
    "Prompts for an application and runs it inside `EXWM'."
    (interactive (list (read-shell-command "> ")))
    (start-process-shell-command command nil command))

  (defun nemacs-exwm-rename-buffer ()
    "Rename the buffers to the window title."
    (exwm-workspace-rename-buffer
     (concat exwm-title " - " (capitalize exwm-class-name))))

  (add-hook 'exwm-update-title-hook #'nemacs-exwm-rename-buffer)
  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)

  (setq exwm-workspace-number 3)

  (setq exwm-input-global-keys
        `(([?\s-&]                . nemacs-exwm-run-application)
          ([?\s-r]                . exwm-reset)

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
