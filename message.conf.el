(require 'smtpmail)

(defvar work-mail-domain "itx.com")

(defvar mail-addresses '("nahueljsacchetti@gmail.com" "nsacchetti@itx.com"))

(defun nemacs-change-mail-address ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "^From: *\\([^<\n]*?\\) *\\(<\\([^>\n]*\\)>\\)?$" nil t)
      (let* ((no-name (null (match-string 2)))
             (name (if no-name user-full-name (match-string 1)))
             (address (match-string (if no-name 1 3))))
        (replace-match (concat "From: " name
                               " <" (nemacs-select-next-address address) ">"))))))
(define-key message-mode-map (kbd "C-c f") #'nemacs-change-mail-address)

(defun nemacs-select-next-address (address)
  (let ((found (member address mail-addresses)))
    (if found
        (if (cdr found)
            (cadr found)
          (car mail-addresses))
      (if (eq last-command this-command)
          (setq current-selected-index
                (mod (+ current-selected-index 1) (length mail-addresses)))
        (setq current-selected-index 0))
      (nth current-selected-index mail-addresses))))

(defun nemacs-check-correct-mail-address ()
  "Checks if sender is the right mail address to prevent sending email to work
from personal mail address."
  (unless (string-match-p work-mail-domain (message-field-value "From"))
    (when (or (string-match-p work-mail-domain (or (message-field-value "To") ""))
              (string-match-p work-mail-domain (or (message-field-value "Cc") "")))
      (if (y-or-n-p "Trying to send an email to work from your personal address. Fix From address? ")
          (nemacs-change-mail-address)
        (keyboard-quit)))))
(add-hook 'message-send-hook #'nemacs-check-correct-mail-address)

(setq message-auto-save-directory (concat nemacs-gnus-dir "/Mail/draft")
      message-confirm-send t
      message-default-mail-headers "Cc: \n"
      message-directory (concat nemacs-gnus-dir "/Mail")
      message-dont-reply-to-names '("nsacchetti" "nahueljsacchetti")
      message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-send-it
      message-signature '(concat "Nahuel Jesús Sacchetti\n"
                                 ";; Technical Leader @ ITX\n"
                                 ";; Email sent from GNUs " gnus-version " and GNU Emacs " emacs-version))

(add-hook 'message-mode-hook #'footnote-mode)
(add-hook 'message-mode-hook #'turn-on-flyspell)
