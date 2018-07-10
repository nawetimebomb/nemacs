(setq gnutls-verify-error t
      mail-from-style nil
      nsm-settings-file (expand-file-name "network-security.data" nemacs-cache-dir)
      send-mail-function 'smtpmail-send-it
      smtpmail-debug-info t
      smtpmail-debug-verb t
      starttls-use-gnutls t
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))
