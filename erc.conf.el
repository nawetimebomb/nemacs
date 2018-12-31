(add-to-list 'erc-modules 'smiley)
(add-to-list 'erc-modules 'match)
(add-to-list 'erc-modules 'scrolltobottom)
(add-to-list 'erc-modules 'notifications)

(defun erc-button-url-previous ()
  "Go to the previous URL button in this buffer."
  (interactive)
  (let* ((point (point))
         (found (catch 'found
                  (while (setq point (previous-single-property-change point 'erc-callback))
                    (when (eq (get-text-property point 'erc-callback) 'browse-url)
                      (throw 'found point))))))
    (if found
        (goto-char found)
      (error "No previous URL button."))))
(define-key erc-mode-map [backtab] 'erc-button-url-previous)

;; Default
(setq erc-fill-prefix "          "
      erc-fill-column 78
      erc-header-line-format "%t: %o"
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-join-buffer 'bury
      erc-server-reconnect-attempts t
      erc-server-reconnect-timeout 10
      erc-timestamp-format "%H:%M "
      erc-warn-about-blank-lines nil)

;; Module: Match
(setq erc-keywords '("elnawe")
      erc-match-exclude-server-buffer t)

;; Module: Notifications
(setq erc-notifications-icon "~/.emacs.d/icons/irc.png")

(require 'my-erc.conf)
