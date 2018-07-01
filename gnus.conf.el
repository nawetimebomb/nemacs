(defvar nemacs-gnus-dir "~/Gnus"
  "Default Gnus directory.")

(setq gnus-directory (concat nemacs-gnus-dir "/News")
      gnus-home-directory nemacs-gnus-dir
      gnus-secondary-select-methods '((nnmaildir "personal"
                                                 (directory "~/Maildir/Personal")
                                                 (directory-files nnheader-directory-files-safe)
                                                 (get-new-mail nil))
                                      (nnmaildir "work"
                                                 (directory "~/Maildir/Work")
                                                 (directory-files nnheader-directory-files-safe)
                                                 (get-new-mail nil)))
      gnus-select-method '(nnnil "")
      nnfolder-directory (concat nemacs-gnus-dir "/Mail"))
