(require 'nnir)
(require 'gnus-select-account)

(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

(gnus-select-account-enable)

(setq gnus-always-read-dribble-file t
      gnus-directory (concat nemacs-gnus-dir "/News")
      gnus-group-goto-unread nil
      gnus-summary-goto-unread 'never
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

(setq-default gnus-summary-line-format "%U%R%z | %d | %( %-23,23f : %B%s%)\n"
              gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
              gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
              gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
              gnus-sum-thread-tree-false-root ""
              gnus-sum-thread-tree-indent "  "
              gnus-sum-thread-tree-leaf-with-other "├► "
              gnus-sum-thread-tree-root "■ "
              gnus-sum-thread-tree-single-leaf "╰► "
              gnus-sum-thread-tree-vertical "│")

(gnus-add-configuration
 '(article (horizontal 1.0 (vertical 25 (group 1.0))
                       (vertical 1.0 (summary 0.25 point) (article 1.0)))))
(gnus-add-configuration
 '(summary (horizontal 1.0 (vertical 25 (group 1.0))
                       (vertical 1.0 (summary 1.0 point)))))

(define-key gnus-group-mode-map (kbd "<return>")
  (lambda ()
    (interactive)
    (gnus-topic-select-group t)))
(define-key gnus-summary-mode-map "F" #'gnus-summary-wide-reply-with-original)
(define-key gnus-article-mode-map "F" #'gnus-article-wide-reply-with-original)

(add-hook 'gnus-startup-hook
          #'(lambda ()
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
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

(zenburn-with-color-variables
  (custom-set-faces
   `(gnus-summary-normal-ancient ((t (:foreground ,zenburn-fg))))
   `(gnus-summary-normal-unread ((t (:foreground ,zenburn-fg+1 :weight bold))))))
