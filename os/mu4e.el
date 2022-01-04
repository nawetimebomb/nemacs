(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")

(use-package boxquote)

(use-package mu4e
  :straight nil
  :preface
  (defun nemacs-setup-mu4e-compose-mode ()
    (flyspell-mode)
    (turn-on-visual-line-mode)
    (use-hard-newlines -1))
  :hook
  (mu4e-compose-mode-hook . nemacs-setup-mu4e-compose-mode)
  :bind
  (("C-x m" . mu4e-compose-new))
  :init
  (require 'boxquote)
  (require 'mu4e-context)
  (require 'mu4e-contrib)
  (require 'mu4e-icalendar)
  (require 'org-mu4e)
  (mu4e-icalendar-setup)
  :config
  (add-to-list 'org-agenda-files "~/Notes/Calendar/diary")
  (add-to-list 'org-agenda-files "~/Notes/Calendar/mu4e")
  (set-variable 'read-mail-command 'mu4e)
  :custom
  (diary-file "~/Notes/Calendar/diary")
  (mail-user-agent 'mu4e-user-agent)
  (message-cite-reply-position 'above)
  (message-cite-style message-cite-style-outlook)
  (message-kill-buffer-on-exit t)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-componse-context-policy 'pick-first)
  (mu4e-componse-dont-reply-to-self t)
  (mu4e-confirm-quit nil)
  (mu4e-context-policy 'pick-first)
  (mu4e-get-mail-command "true")
  (mu4e-html2text-command "w3m -dump -T text/html")
  (mu4e-icalendar-diary-file "~/Notes/Calendar/mu4e")
  (mu4e-icalendar-trash-after-reply t)
  (mu4e-maildir "~/Maildir")
  (mu4e-sent-message-behavior 'delete)
  (mu4e-update-interval 300)
  (mu4e-view-prefer-html t)
  (mu4e-view-show-images t)
  (org-mu4e-convert-to-html t))

(use-package mu4e-alert)
