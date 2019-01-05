(require 'notmuch)
(require 'notmuch-show)

;; Functions
(defun nemacs-notmuch-toggle-delete-tag ()
  "Toggle deleted tag for message. Taken from notmuchmail.org."
  (interactive)
  (if (member "deleted" (notmuch-show-get-tags))
      (notmuch-show-tag (list "-deleted" "+inbox"))
    (notmuch-show-tag (list "+deleted" "-inbox"))))

(defvar notmuch-hello-refresh-count 0)

(defun notmuch-hello-refresh-status-message ()
  (unless no-display
    (let* ((new-count
            (string-to-number
             (car (process-lines notmuch-command "count"))))
           (diff-count (- new-count notmuch-hello-refresh-count)))
      (cond
       ((= notmuch-hello-refresh-count 0)
        (message "You have %s messages."
                 (notmuch-hello-nice-number new-count)))
       ((> diff-count 0)
        (message "You have %s more messages since last refresh."
                 (notmuch-hello-nice-number diff-count)))
       ((< diff-count 0)
        (message "You have %s fewer messages since last refresh."
                 (notmuch-hello-nice-number (- diff-count)))))
      (setq notmuch-hello-refresh-count new-count))))

;; Hooks
(add-hook 'notmuch-search-hook
          (lambda ()
            (interactive)
            (setq cursor-type nil)))
(add-hook 'notmuch-hello-refresh-hook 'notmuch-hello-refresh-status-message)

;; Saved searches
(setq notmuch-saved-searches '((:name "unread from work [uw]"
                                      :key "uw"
                                      :query "tag:work and tag:unread and not tag:bitbucket"
                                      :sort-order 'oldest-first)
                               (:name "unread from bitbucket [ub]"
                                      :key "ub"
                                      :query "tag:unread and tag:bitbucket"
                                      :sort-order 'oldest-first)
                               (:name "all unread [au]"
                                      :key "au"
                                      :query "tag:unread"
                                      :sort-order 'oldest-first)
                               (:name "needs review [nr]"
                                      :key "nr"
                                      :query "tag:needs_review"
                                      :sort-order 'oldest-first)
                               (:name "inbox [i]"
                                      :key "i"
                                      :query "tag:inbox"
                                      :sort-order 'newest-first)
                               (:name "all mail [am]"
                                      :key "am"
                                      :query "*"
                                      :sort-order 'oldest-first)
                               (:name "need update e-mail [nue]"
                                      :key "nue"
                                      :query "tag:need_update_email"
                                      :sort-order 'newest-first)
                               (:name "not tagged (REVIEW) [nt]"
                                      :key "nt"
                                      ;; Run script to get this query updated:
                                      ;; notmuch search --output=tags \* | sed 's/^/not tag:/;2~1s/^/and /'
                                      :query "not tag:inbox and not tag:attachment and not tag:bitbucket and not tag:deleted and not tag:emacs
and not tag:event and not tag:finances and not tag:flagged and not tag:incident and not tag:jira and not tag:mailing_list and not tag:needs_review
and not tag:muted and not tag:need_update_email and not tag:replied and not tag:shopping and not tag:subscription and not tag:work")))

(setq mm-text-html-renderer 'w3m
      mml-enable-flowed nil
      mm-discouraged-alternatives '("text/html" "text/richtext")
      notmuch-multipart/alternative-discouraged '("text/html" "text/richtext")
      notmuch-address-command "notmuch-addrlookup")

;; Keybindings
(global-set-key (kbd "C-x n") #'notmuch)
(global-set-key (kbd "C-x N") (lambda ()
                                (interactive)
                                (notmuch)
                                (notmuch-poll-and-refresh-this-buffer)))

(define-key notmuch-show-mode-map (kbd "C-c C-o") #'browse-url-at-point)

(define-key notmuch-show-mode-map "d"
  (lambda ()
    "Toggle deleted tag for message. Taken from notmuchmail.org."
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-deleted" "+inbox"))
      (notmuch-show-tag (list "+deleted" "-inbox")))))

(define-key notmuch-show-mode-map "r" 'notmuch-show-reply)
(define-key notmuch-show-mode-map "R" 'notmuch-show-reply-sender)

(define-key notmuch-show-mode-map "b"
  (lambda (&optional address)
    "Bounce the current message."
    (interactive "sBounce To: ")
    (notmuch-show-view-raw-message)
    (message-resend address)))

(define-key notmuch-search-mode-map "d"
  (lambda ()
    "Toggle deleted tag for message. Taken from notmuchmail.org."
    (interactive)
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-deleted" "+inbox"))
      (notmuch-search-tag (list "+deleted" "-inbox")))))

(define-key notmuch-search-mode-map "r" 'notmuch-search-reply-to-thread)
(define-key notmuch-search-mode-map "R" 'notmuch-search-reply-to-thread-sender)
(define-key notmuch-tree-mode-map "r" (notmuch-tree-close-message-pane-and #'notmuch-show-reply))
(define-key notmuch-tree-mode-map "R" (notmuch-tree-close-message-pane-and #'notmuch-show-reply-sender))

;; UI
(custom-set-faces
 '(notmuch-search-flagged-face
   ((t (:foreground "OrangeRed"))))
 '(notmuch-message-summary-face
   ((t (:foreground "white" :inherit mode-line)))))