(message "loading gnus conf")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq user-full-name "Nahuel Jesus Sacchetti"
      user-mail-address "nahueljsacchetti@gmail.com"
      auth-sources '(expand-file-name "~/.authinfo.gpg"))

(setq mm-text-html-renderer 'gnus-w3m
      mm-inline-text-html-with-images t
      mm-inline-large-images nil
      mm-attachment-file-modes 420)

(setq gnus-treat-hide-citation t
      gnus-select-method '(nnnil "")
      gnus-secondary-select-methods '((nnmaildir "Gmail"
                                                 (directory "~/Maildir/gmail")
                                                 (directory-files nnheader-directory-files-safe)
                                                 (get-new-mail nil))
                                      (nnmaildir "Work"
                                                 (directory "~/Maildir/work")
                                                 (directory-files nnheader-directory-files-safe)
                                                 (get-new-mail nil))
                                      (nntp "blaine.gmane.org"))
      gnus-default-adaptive-score-alist '((gnus-unread-mark)
                                          (gnus-ticked-mark (subject 10))
                                          (gnus-killed-mark (subject -5))
                                          (gnus-catchup-mark (subject -1)))
      gnus-use-adaptive-scoring t)
