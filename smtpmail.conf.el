;;; smtpmail.conf.el --- The Nemacs SMTP Mail configuration.

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

(require 'cl)
(require 'message)
(require 'my-smtpmail.conf)

;; Functions
(defun nemacs-change-mail-address ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "^From: *\\([^<\n]*?\\) *\\(<\\([^>\n]*\\)>\\)?$" nil t)
      (let* ((no-name (null (match-string 2)))
             (name (if no-name user-full-name (match-string 1)))
             (address (match-string (if no-name 1 3))))
        (replace-match (concat "From: " (nemacs-select-next-address address)
                               " (" name ")"))))))

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

(define-key message-mode-map (kbd "C-c f") #'nemacs-change-mail-address)

;; Hooks
(add-hook 'message-mode-hook
          #'(lambda ()
              (interactive)
              (flyspell-mode)
              (turn-off-auto-fill)))

;; Defaults
(setq gnutls-verify-error t
      mail-from-style nil
      message-auto-save-directory "~/mail/draft"
      message-default-mail-headers "Cc: \n"
      message-directory "~/mail"
      message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-send-it
      nsm-settings-file (expand-file-name "network-security.data" nemacs-cache-dir)
      send-mail-function 'smtpmail-send-it
      smtpmail-debug-info t
      smtpmail-debug-verb t
      starttls-use-gnutls t
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(zenburn-with-color-variables
  (custom-set-faces
   `(message-header-name ((t (:foreground ,zenburn-cyan :weight bold))))
   `(message-header-other ((t (:foreground ,zenburn-yellow))))
   `(message-header-to ((t (:foreground ,zenburn-yellow :weight normal))))
   `(message-header-cc ((t (:foreground ,zenburn-yellow :weight normal))))
   `(message-header-fcc ((t (:foreground ,zenburn-yellow :weight normal))))
   `(message-header-subject ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-mml ((t (:foreground ,zenburn-green+2 :weight normal))))
   `(message-separator ((t (:foreground ,zenburn-orange :weight bold))))))
