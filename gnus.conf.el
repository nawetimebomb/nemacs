;;; gnus.conf.el --- The Nemacs Gnus configuration.

;; Copyright (C) 2017 ~ 2018 Nahuel Jesús Sacchetti <nahueljsacchetti@gmail.com>

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'gnus-cite)
(require 'gnus-select-account)

;;; Start GNUs configuration

;; Startup
(gnus-select-account-enable)

;; Functions
(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

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
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-article-display-hook 'gnus-article-highlight-citation)
(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

;; Defaults
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

(setq gnus-auto-select-next nil
      gnus-group-goto-unread nil
      gnus-large-newsgroup 600
      gnus-permanently-visible-groups "INBOX"
      gnus-read-active-file 'some
      gnus-select-method '(nnnil "")
      gnus-secondary-select-methods '((nnimap "Gmail"
                                              (nnimap-address "imap.gmail.com"))
                                      (nnimap "ITX"
                                              (nnimap-address "outlook.office365.com")))
      gnus-summary-goto-unread 'never)

;; Keybindings
(define-key gnus-group-mode-map (kbd "<return>")
  (lambda ()
    (interactive)
    (gnus-topic-select-group t)))
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

;; FORMAT=FLOWED fix
(defun harden-newlines ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (put-text-property (1- (point)) (point) 'hard t))))

(setq fill-flowed-display-column nil)

;; The following line is needed since emacs 24.1:
(setq gnus-treat-fill-long-lines nil)

(add-hook 'message-setup-hook
  (lambda ()
    (when message-this-is-mail
      (turn-off-auto-fill)
      (setq
	truncate-lines nil
	word-wrap t
	use-hard-newlines t))))

(add-hook 'message-send-hook
  (lambda ()
    (when use-hard-newlines
      (harden-newlines))))

(add-hook 'gnus-article-mode-hook
  (lambda ()
    (setq
      truncate-lines nil
      word-wrap t)))
