(setq bbdb-file (concat nemacs-shared-dir "contacts/bbdb")
      bbdb-update-records-p 'create
      bbdb-mua-pop-up nil
      bbdb-silent t
      bbdb-user-mail-address-re "sacchetti"
      bbdb-add-name t
      bbdb-add-aka t
      bbdb-add-mails t
      bbdb-new-mails-primary t
      bbdb-complete-mail-allow-cycling t
      bbdb-ignore-message-alist '(("From" . "noreply@itx.com")
                                  ("From" . "mailer-daemon")
                                  ("From" . "plus.google.com")
                                  ("From" . "notify@twitter.com")
                                  ("From" . "nsacchetti@itx.com")
                                  ("From" . "nahueljsacchetti@gmail.com"))
      bbdb-allow-duplicates t)
