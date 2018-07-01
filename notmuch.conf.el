(require 'smtpmail)

(defun nemacs-notmuch-toggle-deleted-tag ()
  "Toggles deleted tag on a thread or message and removes it from the inbox."
  (interactive)
  (when (equal major-mode 'notmuch-search-mode)
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag '("-deleted" "+inbox"))
      (notmuch-search-tag '("+deleted" "-inbox" "-unread"))))
  (when (equal major-mode 'notmuch-show-mode)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag '("-deleted" "+inbox"))
      (notmuch-show-tag '("+deleted" "-inbox" "-unread")))))

(defun nemacs-notmuch-poll-and-refresh ()
  "Poll my notmuch server and refreshes the page I'm currently on.
Shows the results on another buffer or the echo area."
  (interactive)
  (shell-command "~/dotfiles/bin/poll-mail")
  (notmuch-poll-and-refresh-this-buffer))

(setq notmuch-always-prompt-for-sender t
      notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                               (:name "unread" :query "tag:unread" :key "u")
                               (:name "flagged" :query "tag:flagged" :key "f")
                               (:name "sent" :query "tag:sent" :key "t")
                               (:name "deleted" :query "tag:deleted"))
      notmuch-search-oldest-first nil)

(define-key notmuch-hello-mode-map "g" #'nemacs-notmuch-poll-and-refresh)
(define-key notmuch-search-mode-map "d" #'nemacs-notmuch-toggle-deleted-tag)
(define-key notmuch-show-mode-map "d" #'nemacs-notmuch-toggle-deleted-tag)

(define-key notmuch-hello-mode-map "i"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:inbox")))

(define-key notmuch-hello-mode-map "u"
  (lambda ()
    (interactive)
    (notmuch-hello-search "tag:unread")))
