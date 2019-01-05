(require 'dashboard)

(defun nemacs-dashboard-banner ()
  "Set a dashboard banner including information on package initialization
     time and garbage collections."
  (setq dashboard-banner-logo-title
        (format "Emacs ready in %.2f seconds with %d garbage collections."
                (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

(add-hook 'after-init-hook 'dashboard-refresh-buffer)
(add-hook 'dashboard-mode-hook 'nemacs-dashboard-banner)

(setq dashboard-startup-banner 'logo)

(dashboard-setup-startup-hook)
