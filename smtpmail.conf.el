(require 'cl)
(require 'smtpmail)

(setq mail-addresses '("nahueljsacchetti@gmail.com" "nsacchetti@itx.com"))

(defun nemacs-change-mail-address ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "^From: *\\([^<\n]*?\\) *\\(<\\([^>\n]*\\)>\\)?$" nil t)
      (let* ((no-name (null (match-string 2)))
             (name (if no-name user-full-name (match-string 1)))
             (address (match-string (if no-name 1 3))))
        (replace-match (concat "From: " name
                               " <" (nemacs-select-next-address address) ">"))))))

(defun nemacs-select-next-address (address)
  (let ((found (member address mail-addresses)))
    (if found
        (if (cdr found)
            (cadr found)
          (car bsb/email-addresses))
      (if (eq last-command this-command)
          (setq current-selected-index
                (mod (+ current-selected-index 1) (length mail-addresses)))
        (setq current-selected-index 0))
      (nth current-selected-index bsb/email-addresses))))

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
        smtpmail-auth-credentials (list (list server port user password))
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

(setq gnutls-verify-error t
      mail-from-style nil
      message-auto-save-directory (concat nemacs-gnus-dir "/Mail/draft")
      message-default-mail-headers "Cc: \n"
      message-directory (concat nemacs-gnus-dir "/Mail")
      message-kill-buffer-on-exit t
      message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-send-it
      nsm-settings-file (expand-file-name "network-security.data" nemacs-cache-dir)
      send-mail-function 'smtpmail-send-it
      smtpmail-debug-info t
      smtpmail-debug-verb t
      starttls-use-gnutls t
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
      user-full-name "Nahuel Jesús Sacchetti"
      user-mail-address "nahueljsacchetti@gmail.com")

(define-key message-mode-map (kbd "C-c f") #'nemacs-change-mail-address)
