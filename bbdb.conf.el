(setq bbdb-file "~/Notes/contacts/bbdb"
      bbdb-update-records-p 'create
      bbdb-mua-pop-up nil
      bbdb-silent t
      bbdb-user-mail-address-re "nahueljsacchetti"
      bbdb-add-name t
      bbdb-add-aka t
      bbdb-add-mails t
      bbdb-new-mails-primary t
      bbdb-complete-mail-allow-cycling t
      bbdb-ignore-message-alist '(("From" . "mailer-daemon")
                                  ("From" . "plus.google.com")
                                  ("From" . "notify@twitter.com"))
      bbdb-allow-duplicates t)
