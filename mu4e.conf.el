(mu4e-alert-enable-notifications)
(mu4e-alert-enable-mode-line-display)

(set-fontset-font "fontset-default" 'unicode "Dejavu Sans Mono")

(setq mu4e-maildir "~/Maildir"
      mu4e-get-mail-command "~/dotfiles/bin/retrieve-mail"
      mu4e-change-filenames-when-moving t ;; better for mbsync
      mu4e-context-policy 'pick-first
      mu4e-confirm-quit nil
      mu4e-use-fancy-chars nil
      mu4e-view-show-addresses t)

(require 'bbdb-mu4e)
(setq mu4e-compose-complete-addresses nil)

(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)

(add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)

(setq mu4e-view-show-images t
      mu4e-show-images t
      mu4e-view-image-max-width 800
      mu4e-html2text-command "w3m -dump -T text/html")

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Personal"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "nahueljsacchetti@gmail.com")))
           :vars '((user-mail-address      . "nahueljsacchetti@gmail.com")
                   (user-full-name         . "Nahuel Jesús Sacchetti")
                   (mu4e-compose-signature . (concat "Nahuel Jesús Sacchetti
"                                                    ";; Senior JavaScript Developer
"                                                    ";; Enthusiastic Game Developer
"                                                    ";; Emacs extraordinaire
"                                                    ";; This message was sent from GNU Emacs "
                                                     emacs-version))
                   (mu4e-drafts-folder     . "/personal/drafts")
                   (mu4e-refile-folder     . "/personal/archive")
                   (mu4e-sent-folder       . "/personal/sent")
                   (mu4e-trash-folder      . "/personal/trash")
                   (smtpmail-smtp-server   . "smtp.gmail.com")
                   (smtpmail-smtp-service  . 587)
                   (smtpmail-stream-type   . starttls)))
         ,(make-mu4e-context
           :name "Work"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "nsacchetti@itx.com")))
           :vars '((user-mail-address      . "nsacchetti@itx.com")
                   (user-full-name         . "Nahuel Jesús Sacchetti")
                   (mu4e-compose-signature . (concat "Nahuel Jesús Sacchetti
"                                                    ";; Technical Leader @ ITX
"                                                    ";; Message sent from GNU Emacs "
                                                     emacs-version))
                   (mu4e-drafts-folder     . "/work/drafts")
                   (mu4e-refile-folder     . "/work/archive")
                   (mu4e-sent-folder       . "/work/sent")
                   (mu4e-trash-folder      . "/work/trash")
                   (smtpmail-smtp-server   . "smtp.office365.com")
                   (smtpmail-smtp-service  . 587)
                   (smtpmail-stream-type   . starttls)))))

(setq mu4e-sent-messages-behavior 'sent
      mu4e-compose-format-flowed t
      mu4e-update-interval (* 15 60)
      mu4e-headers-fields '((:human-date      . 12)
                            (:flags           . 6)
                            (:from            . 30)
                            (:thread-subject . nil))
      mu4e-maildir-shortcuts '(("/personal/INBOX" . ?p)
                               ("/work/INBOX"     . ?w)))

(add-hook 'mu4e-compose-mode-hook #'footnote-mode)
(add-hook 'mu4e-compose-mode-hook #'turn-on-flyspell)


(zenburn-with-color-variables
  (custom-set-faces
   `(mu4e-header-highlight-face ((t (:inherit region :underline nil :weight bold))))))
