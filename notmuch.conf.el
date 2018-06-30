(require 'cl)
(require 'smtpmail)

(defvar smtp-accounts
  '((ssl "nahueljsacchetti@gmail.com" "smtp.gmail.com" 25 "nahueljsacchetti@gmail.com" nil)
    (ssl "nsacchetti@itx.com" "smtp.office365.com" 587 "nsacchetti@itx.com" nil)))

(defun set-smtp (mech server port user password)
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'." server port user))

(defun set-smtp-ssl (server port user password  &optional key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
        starttls-extra-arguments nil
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        stmpmail-stream-type 'tls
        smtpmail-starttls-credentials (list (list server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)" server port user))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from)
          do (cond
              ((memq auth-mech '(cram-md5 plain login))
               (return (apply 'set-smtp (cons auth-mech auth-spec))))
              ((eql auth-mech 'ssl)
               (return (apply 'set-smtp-ssl auth-spec)))
              (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
          finally (error "Cannot infer SMTP information."))))

(defadvice smtpmail-via-smtp
    (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
  "Call `change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (change-smtp)))

(ad-activate 'smtpmail-via-smtp)

;; SMTP - Sending email
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      user-full-name "Nahuel Jesus Sacchetti"
      smtpmail-debug-info t
      smtpmail-debug-verb t
      starttls-use-gnutls t)

;; IMAP/Notmuch - Reading email
(setq message-auto-save-directory "~/Mail/draft"
      message-kill-buffer-on-exit t
      message-default-mail-headers "Cc: \n"
      message-directory "~/Mail/"
      notmuch-always-prompt-for-sender t
      notmuch-search-oldest-first nil
      notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                               (:name "unread" :query "tag:unread" :key "u")
                               (:name "flagged" :query "tag:flagged" :key "f")
                               (:name "sent" :query "tag:sent" :key "t")
                               (:name "deleted" :query "tag:deleted")))

(global-set-key (kbd "C-c m") #'notmuch)
(global-set-key (kbd "C-x h") #'helm-notmuch)
(global-set-key (kbd "C-x m") (lambda () (interactive) (notmuch-mua-new-mail t)))
(define-key notmuch-search-mode-map "g" 'notmuch-poll-and-refresh-this-buffer)
(define-key notmuch-hello-mode-map "g" 'notmuch-poll-and-refresh-this-buffer)
(define-key notmuch-search-mode-map "d"
  (lambda ()
    "toggle deleted tag for thread"
    (interactive)
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag '("-deleted"))
      (notmuch-search-tag '("+deleted" "-inbox" "-unread")))))

(define-key notmuch-show-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag '("-deleted"))
      (notmuch-show-tag '("+deleted" "-inbox" "-unread")))))

(define-key notmuch-hello-mode-map "i"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:inbox")))

(define-key notmuch-hello-mode-map "u"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:unread")))

(defun nemacs-new-mail-whole-frame (account)
  "Creates new email with `notmuch' but closes other windows before"
  (interactive)
  (delete-other-windows)
  (notmuch)
  (setq user-mail-address account
        smtpmail-mail-address account
        notmuch-always-prompt-for-sender nil)
  (notmuch-mua-new-mail nil))

(defun nemacs-show-unread-messages ()
  "Opens the unread messages in the whole frame"
  (interactive)
  (delete-other-windows)
  (notmuch-hello-search "tag:unread"))
