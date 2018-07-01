(require 'smtpmail)

(setq gnus-directory (concat nemacs-gnus-dir "/News")
      gnus-home-directory nemacs-gnus-dir
      gnus-secondary-select-methods '((nnimap "personal"
                                              (nnimap-stream network)
                                              (nnimap-address "localhost")
                                              (nnimap-server-port 8143)
                                              (nnimap-authenticator login))
                                      (nnimap "work"
                                              (nnimap-stream network)
                                              (nnimap-address "localhost")
                                              (nnimap-server-port 8144)
                                              (nnimap-authenticator login)))
      gnus-select-method '(nnnil "")
      mm-text-html-renderer 'w3m
      nnfolder-directory (concat nemacs-gnus-dir "/Mail"))
