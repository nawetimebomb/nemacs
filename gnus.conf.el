(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq user-mail-address "nahueljsacchetti@gmail.com"
      auth-sources '(expand-file-name "~/.authinfo.gpg"))

(setq mm-text-html-renderer 'gnus-w3m
      mm-inline-text-html-with-images t
      mm-inline-large-images nil
      mm-attachment-file-modes 420)

(setq gnus-ignored-newsgroups "Spam"
      gnus-treat-hide-citation t
      gnus-select-method '(nnnil "")
      gnus-secondary-select-methods '((nnimap "gmail"
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-server-port 993)
                                              (nnimap-stream ssl))
                                      (nnimap "work"
                                              (nnimap-address "outlook.office365.com")
                                              (nnimap-server-port 993)
                                              (nnimap-stream ssl))
                                      (nntp "blaine.gmane.org"))
      gnus-default-adaptive-score-alist '((gnus-unread-mark)
                                          (gnus-ticked-mark (subject 10))
                                          (gnus-killed-mark (subject -5))
                                          (gnus-catchup-mark (subject -1)))
      gnus-use-adaptive-scoring t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; multiple outgoing accounts ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.mostlymaths.net/2010/12/emacs-30-day-challenge-using-gnus-to.html
;; also see ~/.authinfo
(defvar smtp-accounts '((ssl "nahueljsacchetti@gmail.com" "smtp.gmail.com" 587 "nahueljsacchetti@gmail.com" nil)
                        (ssl "nsacchetti@itx.com" "smtp.office365.com" 587 "nsacchetti@itx.com" "ppjgzhfdzjlkpxcw")))

;; Now lets configure smtpmail.el with your name and functions to send
;; mail using your smtp accounts by changing the from field
(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil user-full-name "Nahuel Jesús Sacchetti"
      smtpmail-debug-info t smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user
                                              password)) smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun set-smtp-ssl (server port user password &optional key
                            cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user
                                              password)) smtpmail-starttls-credentials (list (list
                                              server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL
enabled.)" server port user))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (cl-loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from) do (cond
                                               ((memq auth-mech '(cram-md5 plain login))
                                                (cl-return (apply 'set-smtp (cons auth-mech auth-spec))))
                                               ((eql auth-mech 'ssl)
                                                (cl-return (apply 'set-smtp-ssl auth-spec)))
                                               (t (error "Unrecognized SMTP auth. mechanism:
`%s'." auth-mech))) finally (error "Cannot infer SMTP
information."))))

;; The previous function will complain if you fill the from field with;; an account not present in smtp-accounts.
(defvar %smtpmail-via-smtp (symbol-function 'smtpmail-via-smtp))

(defun smtpmail-via-smtp (recipient smtpmail-text-buffer)
  (with-current-buffer smtpmail-text-buffer
    (change-smtp))
  (funcall (symbol-value '%smtpmail-via-smtp) recipient
           smtpmail-text-buffer))

;; This wraps send mail via smtp mail, to be able to send multiple
;; messages with smtpmail.

;; Reply-to with same address it was sent to
(setq gnus-posting-styles
      '(((header "to" "nahueljsacchetti@gmail.com")
         (address "nahueljsacchetti@gmail.com"))
        ((header "to" "nsacchetti@itx.com")
         (address "nsacchetti@itx.com"))))
