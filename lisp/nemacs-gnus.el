(require 'nnir)
(require 'gnus-cite)
(require 'gnus-select-account)

;;; Start GNUs configuration

;; Startup
(gnus-select-account-enable)

;; Functions
(defun exit-gnus-on-exit ()
  "Exit Gnus before exiting Emacs."
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

(defun nemacs-gnus-cursor-mode ()
  "Hide teh cursor and highlight the line."
  (interactive)
  (hl-line-mode)
  (setq cursor-type nil))

;; Hooks
(add-hook 'gnus-startup-hook
          #'(lambda ()
              (gnus-demon-init)
              (setq gnus-demon-timestep 60)
              ;; Update every 15 minutes
              (gnus-demon-add-handler '(lambda ()
                                         (gnus-demon-scan-news)
                                         (message "NEMACS GNUs: Checking email...")
                                         (gnus-group-save-newsrc))
                                      15 nil)
              (gnus-demon-add-scanmail)

              ;; Don't crash gnus if disconnected (laptop)
              (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
                "Timeout for Gnus."
                (with-timeout (120 (message "Gnus timed out.")) ad-do-it))))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-summary-mode-hook 'nemacs-gnus-cursor-mode)
(add-hook 'gnus-article-display-hook 'gnus-article-highlight-citation)
(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)
(add-hook 'gnus-article-display-hook 'gnus-article-highlight)
(add-hook 'gnus-article-display-hook 'gnus-article-hide-headers-if-wanted)
(add-hook 'gnus-article-display-hook 'gnus-article-hide-boring-headers)
(add-hook 'gnus-article-display-hook 'gnus-article-de-quoted-unreadable)
(add-hook 'gnus-article-display-hook 'gnus-article-strip-leading-blank-lines)
(add-hook 'gnus-article-display-hook 'gnus-article-remove-trailing-blank-lines)
(add-hook 'gnus-article-display-hook 'gnus-article-strip-multiple-blank-lines)
(add-hook 'gnus-article-display-hook 'gnus-article-emphasize)


;; Defaults
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods '((nnimap "Gmail"
                                              (nnimap-address "imap.gmail.com")
                                              (nnir-search-engine imap)
                                              (nnmail-expiry-target "nnimap+Gmail:[Gmail]/Trash")
                                              (nnmail-expiry-wait 90))
                                      (nnimap "ITX"
                                              (nnimap-address "outlook.office365.com")
                                              (nnir-search-engine imap)
                                              (nnmail-expiry-target "nnimap+ITX:Deleted Items")
                                              (nnmail-expiry-wait 90))))

(setq-default gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
              gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today)
         . "Today %H:%M")
        ((+ 86400
            (gnus-seconds-today))
         . "Yesterday")
        (604800 . "%A %d")
        ((gnus-seconds-month)
         . "%A %d")
        ((gnus-seconds-year)
         . "%a-%d")
        (t . "%d-%m-%Y")))

(setq gnus-summary-line-format "%U%R%z | %12&user-date; | %(%-23,23f %*: %B%s%)\n")
(setq gnus-group-line-format "%S%p%P%M%5y: %(%B%g%B%)\n")
(setq gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v\n")


(setq gnus-sum-thread-tree-false-root      ""
      gnus-sum-thread-tree-single-indent   ""
      gnus-sum-thread-tree-root            ""
      gnus-sum-thread-tree-vertical        "|"
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-single-leaf     "\\-> "
      gnus-sum-thread-tree-indent          " ")

(setq gnus-asynchronous t
      gnus-use-cache t
      gnus-read-active-file 'some
      gnus-auto-select-next nil
      gnus-group-goto-unread nil
      gnus-large-newsgroup 600
      gnus-save-killed-list nil
      gnus-check-new-newsgroups 'ask-server
      gnus-permanently-visible-groups "INBOX"
      gnus-read-active-file 'some
      gnus-summary-goto-unread 'never)

(when (executable-find "w3m") (setq mm-text-html-renderer 'gnus-w3m))

;; Keybindings
(define-key gnus-summary-mode-map "F" #'gnus-summary-wide-reply-with-original)
(define-key gnus-article-mode-map "F" #'gnus-article-wide-reply-with-original)

;; UI
(zenburn-with-color-variables
  (custom-set-faces

   ;; Group
   `(gnus-group-mail-3-empty ((t (:foreground ,zenburn-fg-1))))
   `(gnus-group-mail-3 ((t (:foreground ,zenburn-orange))))

   ;; Summary
   `(gnus-summary-normal-ticked ((t (:foreground ,zenburn-cyan :weight normal))))
   `(gnus-summary-normal-ancient ((t (:foreground ,zenburn-fg-1))))
   `(gnus-summary-normal-unread ((t (:foreground ,zenburn-fg+1 :weight bold))))
   `(gnus-summary-normal-read ((t (:foreground ,zenburn-fg))))
   `(gnus-summary-selected ((t (:background ,zenburn-bg+05))))

   ;; Article
   `(gnus-header-name ((t (:foreground ,zenburn-fg-1 :weight bold))))
   `(gnus-header-content ((t (:foreground ,zenburn-fg-1))))
   `(gnus-header-subject ((t (:foreground ,zenburn-cyan))))

   `(variable-pitch ((t (:font unspecified))))))
