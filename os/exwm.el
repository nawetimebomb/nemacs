;;; EXWM
;; NOTE: This configuration looks different from the others, everything is inside `use-package:config'.
;;       The reason for that is that EXWM is very sensible on how we initialize and change variables and
;;       most of then need to be initialized before running `(exwm-enable)'. Hopefully, this is the only
;;       configuration that is not following "the rule" (although there's no rule imposed, really).

(use-package desktop-environment
  :after exwm
  :config
  (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-get-command "light")
  (desktop-environment-brightness-set-command "light %s")
  (desktop-environment-brightness-get-regexp "^\\([0-9]+\\)")
  (desktop-environment-brightness-normal-increment "-A 10")
  (desktop-environment-brightness-normal-decrement "-U 10")
  (desktop-environment-brightness-small-increment "-A 5")
  (desktop-environment-brightness-small-decrement "-U 5"))

(use-package exwm
  :config
  (use-package exwm-edit)

  (require 'exwm-edit)
  (require 'exwm-systemtray)

  ;; Function utilities
  (defun nemacs-run-in-background (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defun nemacs-exwm-init-hook ()
    ;; Start in Workspace 1
    (exwm-workspace-switch-create 1)

    (setq display-time-day-and-date t)

    ;; Enable system modes
    (display-time-mode)
    (display-battery-mode)

    ;; Startup Script
    (start-process-shell-command "sh" nil "sh ~/.emacs.d/os/startup.sh")

    (nemacs-run-in-background "nm-applet")
    (nemacs-run-in-background "pasystray")
    (nemacs-run-in-background "blueman-applet"))

  (defun nemacs-exwm-run-application (command)
    "Prompts for an application and runs it inside `EXWM'."
    (interactive (list (read-shell-command "> ")))
    (start-process-shell-command command nil command))

  (defun nemacs-exwm-rename-buffer ()
    "Rename the buffers to the window title."
    (exwm-workspace-rename-buffer
     (concat exwm-title " - " (capitalize exwm-class-name))))

  (defun nemacs-exwm-lock-computer ()
    (interactive)
    (nemacs-exwm-run-application "slock"))

  ;; Hooks
  (add-hook 'exwm-init-hook #'nemacs-exwm-init-hook)
  (add-hook 'exwm-update-title-hook #'nemacs-exwm-rename-buffer)
  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)

  ;; Configurations
  (setq exwm-workspace-number 3
        exwm-systemtray-height 22)

  (setq exwm-input-global-keys
        `(([?\s-&]                . nemacs-exwm-run-application)
          ([?\s-r]                . exwm-reset)
          ([?\s-w]                . exwm-workspace-switch)

          ([?\s-l]                . nemacs-exwm-lock-computer)

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

  (setq exwm-input-simulation-keys
        '(([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ([?\C-s] . [?\C-f])
          ([?\C-k] . [S-end delete])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])))

  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  (exwm-edit-mode)
  (exwm-systemtray-enable)
  (exwm-enable))
