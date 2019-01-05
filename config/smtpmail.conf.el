(use-package smtpmail
  :ensure nil
  :preface
  (defvar mail-addresses '("nsacchetti@itx.com" "me@nsacchetti"))

  (defun nemacs-change-mail-address ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             "^From: *\\([^<\n]*?\\) *\\(<\\([^>\n]*\\)>\\)?$" nil t)
        (let* ((no-name (null (match-string 2)))
               (name (if no-name user-full-name (match-string 1)))
               (address (match-string (if no-name 1 3))))
          (replace-match (concat "From: " (nemacs-select-next-address address)
                                 " (" name ")"))))))

  (defun nemacs-select-next-address (address)
    (let ((found (member address mail-addresses)))
      (if found
          (if (cdr found)
              (cadr found)
            (car mail-addresses))
        (if (eq last-command this-command)
            (setq current-selected-index
                  (mod (+ current-selected-index 1) (length mail-addresses)))
          (setq current-selected-index 0))
        (nth current-selected-index mail-addresses))))
  
  (defun nemacs-my-signature ()
    (concat
     "Nahuel Jesús Sacchetti\n"
     "Solution Lead for Monsters at ITX\n"
     "Learn more about ITX at https://www.itx.com/\n"
     "and about me at https://nsacchetti.com\n\n"

     "Message sent from GNU Emacs " emacs-version "."))
  :bind (:map message-mode-map
              ("C-c f" . nemacs-change-mail-address))
  :custom
  (message-confirm-send t)
  (gnutls-verify-error t)
  (mail-from-style nil)
  (message-directory "~/mail")
  (message-auto-save-directory "~/mail/drafts")
  (smtpmail-queue-dir "~/mail/queued-mail")
  (message-default-mail-headers "Cc: \n")
  (message-kill-buffer-on-exit t)
  (message-send-mail-function 'smtpmail-send-it)
  (nsm-settings-file (expand-file-name "network-security.data" nemacs-cache-dir))
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-debug-info t)
  (smtpmail-debug-verb t)
  (starttls-use-gnutls t)
  (tls-checktrust gnutls-verify-error)
  (tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                     ;; compatibility fallbacks
                     "gnutls-cli -p %p %h"
                     "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")))
