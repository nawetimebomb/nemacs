(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")

(use-package boxquote)

(use-package mu4e
  :straight nil
  :defer 60
  :preface
  (defun nemacs-setup-mu4e-compose-mode ()
    (flyspell-mode)
    (turn-on-visual-line-mode)
    (use-hard-newlines -1))
  :hook
  (mu4e-compose-mode-hook . nemacs-setup-mu4e-compose-mode)
  :bind
  (("C-x m" . mu4e-compose-new)
   ("C-x M" . mu4e))
  :init
  (require 'boxquote)
  (require 'mu4e-context)
  (require 'mu4e-contrib)
  (require 'mu4e-icalendar)
  (require 'org-mu4e)
  :config
  (add-to-list 'org-agenda-files "~/Notes/Calendar/mu4e.org")
  (set-variable 'read-mail-command 'mu4e)

  (setq gnus-icalendar-org-capture-file "~/Notes/Calendar/mu4e.org"
        gnus-icalendar-org-capture-headline '("SWA")
        mu4e-icalendar-diary-file "~/Notes/Calendar/mu4e.org"
        mu4e-icalendar-trash-after-reply t)
  (mu4e-icalendar-setup)
  :custom
  (diary-file "~/Notes/Calendar/diary")
  (mail-user-agent 'mu4e-user-agent)
  (message-cite-reply-position 'above)
  (message-cite-style message-cite-style-outlook)
  (message-kill-buffer-on-exit t)
  (mm-text-html-renderer 'w3m)
  (mm-w3m-safe-url-regexp nil)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-change-filenames-when-moving t)
  (mu4e-compose-context-policy 'pick-first)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-confirm-quit nil)
  (mu4e-context-policy 'pick-first)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-html2text-command "w3m -dump -T text/html")
  (mu4e-maildir "~/Maildir")
  (mu4e-sent-message-behavior 'delete)
  (mu4e-update-interval (* 10 60))
  (mu4e-view-prefer-html t)
  (mu4e-view-show-images t)
  (org-mu4e-convert-to-html t)
  (shr-use-colors nil))

(use-package mu4e-alert
  :config
  (mu4e-alert-enable-mode-line-display))

(use-package w3m)
