(use-package boxquote)

(use-package message
  :ensure nil
  :preface
  (defun nemacs-boxquote-region-or-input ()
    (interactive)
    (if (use-region-p)
        (boxquote-region (region-beginning) (region-end))
      (call-interactively #'boxquote-text)))
  :bind (:map message-mode-map
              ("M-q"   . nemacs-boxquote-region-or-input)
              ("C-M-y" . boxquote-yank)
              ("M-Q"   . boxquote-title)))

(use-package mu4e
  :ensure nil
  :ensure-system-package mu
  :bind (("C-x m" . mu4e)
         ("C-x M" . mu4e-compose-new))
  :preface
  (require 'mu4e) ;; required, use-package isn't autoloading stuff.

  (defun nemacs-headers-style ()
    (setq cursor-style nil))

  (defun nemacs-my-signature ()
    (concat
     "Nahuel Jesús Sacchetti\n"
     "Solution Lead for Monsters at ITX\n"
     "Learn more about ITX at https://www.itx.com/\n"
     "and about me at https://nsacchetti.com\n\n"

     "Message sent from GNU Emacs " emacs-version "."))
  :hook (mu4e-headers-mode . nemacs-headers-style)
  :custom
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function 'ivy-completing-read)
  (mu4e-compose-context-policy 'ask)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-signature-auto-include t)
  (mu4e-confirm-quit nil)
  (mu4e-context-policy 'pick-first)
  (mu4e-contexts `(,(make-mu4e-context
                     :name "itx.com"
                     :match-func (lambda (msg)
                                   (or (when msg (mu4e-message-contact-field-matches msg :to "nsacchetti@itx.com"))
                                       (when msg (mu4e-message-contact-field-matches msg :from "nsacchetti@itx.com"))))
                     :vars '((user-mail-address      . "nsacchetti@itx.com")
                             (user-full-name         . "Nahuel Jesús Sacchetti")
                             (mu4e-compose-signature . (nemacs-my-signature))
                             (mu4e-refile-folder     . "/archive/itx.com/")
                             (mu4e-trash-folder      . "/trash/itx.com/")
                             (smtpmail-smtp-server   . "smtp.fastmail.com")
                             (smtpmail-smtp-service  . 465)
                             (smtpmail-stream-type   . ssl)))
                   ,(make-mu4e-context
                     :name "nsacchetti.com"
                     :match-func (lambda (msg)
                                   (or (when msg (mu4e-message-contact-field-matches msg :to "me@nsacchetti.com"))
                                       (when msg (mu4e-message-contact-field-matches msg :from "me@nsacchetti.com"))))
                     :vars '((user-mail-address      . "me@nsacchetti.com")
                             (user-full-name         . "Nahuel Jesús Sacchetti")
                             (mu4e-compose-signature . (nemacs-my-signature))
                             (mu4e-refile-folder     . "/archive/nsacchetti.com/")
                             (mu4e-trash-folder      . "/trash/nsacchetti.com/")
                             (smtpmail-smtp-server   . "smtp.fastmail.com")
                             (smtpmail-smtp-service  . 465)
                             (smtpmail-stream-type   . ssl)))))
  (mu4e-drafts-folder "/drafts")
  (mu4e-get-mail-command (format "mbsync -a"))
  (mu4e-headers-fields '((:human-date     .  13)
                         (:flags          .   6)
                         (:from           .  30)
                         (:thread-subject . nil)))
  (mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  (mu4e-maildir "~/mail")
  (mu4e-maildir-shortcuts '(("/nsacchetti.com" . ?n)
                            ("/itx.com" . ?i)))
  (mu4e-org-contacts-file "~/Dropbox/orgfiles/contacts.org")
  (mu4e-sent-folder "/sent")
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-update-interval 3600)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  :config
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)
  :custom-face
  (mu4e-context-face ((t (:foreground "white" :weight bold))))
  (mu4e-modeline-face ((t (:foreground "white"))))
  (mu4e-unread-face ((t (:foreground "navy" :weight bold))))
  (mu4e-highlight-face ((t (:inherit default :weight bold))))
  (mu4e-header-highlight-face ((t (:inherit region :underline nil)))))
