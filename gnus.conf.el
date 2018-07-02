(require 'nnir)
(require 'smtpmail)
(require 'gnus-select-account)

(gnus-select-account-enable)

(setq gnus-always-read-dribble-file t
      gnus-directory (concat nemacs-gnus-dir "/News")
      gnus-group-goto-unread nil
      gnus-home-directory nemacs-gnus-dir
      gnus-save-newsrc-file nil
      gnus-secondary-select-methods '((nnimap "personal"
                                              (nnimap-stream network)
                                              (nnimap-address "localhost")
                                              (nnimap-server-port 8143)
                                              (nnimap-authenticator login)
                                              (nnir-search-engine imap))
                                      (nnimap "work"
                                              (nnimap-stream network)
                                              (nnimap-address "localhost")
                                              (nnimap-server-port 8144)
                                              (nnimap-authenticator login)
                                              (nnir-search-engine imap)))
      gnus-select-method '(nnnil "")
      mm-text-html-renderer 'w3m
      nnfolder-directory (concat nemacs-gnus-dir "/Mail"))

(add-hook 'gnus-startup-hook #'(lambda ()
                                 (gnus-demon-init)
                                 (setq gnus-demon-timestep 60)
                                 ;; Update every 15 minutes
                                 (gnus-demon-add-handler '(lambda ()
                                                            (gnus-demon-scan-news)
                                                            (message "NEMACS GNUs: Checking email..."))
                                                         15 nil)

                                 (require 'gnus-desktop-notify)
                                 (gnus-desktop-notify-mode)
                                 (gnus-demon-add-scanmail)

                                 ;; Don't crash gnus if disconnected (laptop)
                                 (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
                                   "Timeout for Gnus."
                                   (with-timeout (120 (message "Gnus timed out.")) ad-do-it))))
