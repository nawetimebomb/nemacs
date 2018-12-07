;;; nemacs-email.el --- My email setup.

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

;; Load Wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-user-agent-compose "wl-draft" nil t)

;; Hooks
(add-hook 'wl-summary-mode-hook '(lambda () (hl-line-mode t)))
(if (boundp 'mail-user-agent) (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent) (define-mail-user-agent
                                        'wl-user-agent
                                        'wl-user-agent-compose
                                        'wl-draft-send
                                        'wl-draft-kill
                                        'mail-send-hook))

;; Defaults
(setq elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-default-user "nahueljsacchetti@gmail.com"
      elmo-imap4-use-cache t
      elmo-imap4-use-modified-utf7 t
      elmo-message-fetch-confirm nil
      wl-biff-check-interval 180
      wl-biff-check-delay nil
      wl-default-folder "%inbox"
      wl-default-spec "%"
      wl-folder-check-async t
      wl-folder-window-width 30
      wl-folders-file (concat nemacs-emacs-dir "wl/folders")
      wl-from "Nahuel Jesús Sacchetti <nahueljsacchetti@gmail.com>"
      wl-prefetch-confirm nil
      wl-stay-folder-window t)

;; Accounts & Templates
(setq wl-draft-config-alist '(((string-match "itx.com" wl-draft-parent-folder)
                               (template . "WORK"))
                              ((string-match "gmail.com" wl-draft-parent-folder)
                               (template . "GMAIL"))

        ;; automatic for replies
                              (reply "\\(To\\|Cc\\|Delivered-To\\): .*itx.com.*"
                                     (template . "WORK"))
                              (reply "\\(To\\|Cc\\|Delivered-To\\): .*gmail.com.*"
                                     (template . "GMAIL")))
      wl-template-alist '(("GMAIL"
                           (wl-draft-folder . "%[Gmail]/Drafts")
                           (wl-trash-folder . "%[Gmail]/Trash")
                           (wl-from . "Nahuel Jesús Sacchetti <nahueljsacchetti@gmail.com>")
                           (wl-smtp-posting-user . "nahueljsacchetti")
                           (wl-smtp-posting-server . "smtp.gmail.com")
                           (wl-smtp-authenticate-type . "plain")
                           (wl-smtp-connection-type . 'starttls)
                           (wl-local-domain . "gmail.com")
                           (wl-smtp-posting-port . 587)
                           ("From" . wl-from))
                          ("WORK"
                           (wl-draft-folder . "%Drafts")
                           (wl-trash-folder . "%Trash")
                           (wl-from . "Nahuel Jesús Sacchetti <nsacchetti@itx.com>")
                           (wl-smtp-posting-user . "nsacchetti@itx.com")
                           (wl-smtp-posting-server . "smtp.office365.com")
                           (wl-smtp-authenticate-type . "login")
                           (wl-smtp-connection-type . 'starttls)
                           (wl-local-domain . "itx.com")
                           (wl-smtp-posting-port . 587)
                           ("From" . wl-from)
                           ("Organization" . "ITX")))
      wl-user-mail-address-list '("nahueljsacchetti@gmail.com" "nsacchetti@itx.com"))

;; Layout & UI
(setq wl-message-ignored-field-list '("^.*:")
      wl-message-sort-field-list '("^From"
                                   "^Organization:"
                                   "^X-Attribution:"
                                   "^Subject"
                                   "^Date"
                                   "^To"
                                   "^Cc")
      wl-message-visible-field-list '("^\\(To\\|Cc\\):"
                                      "^Subject:"
                                      "^\\(From\\|Reply-To\\):"
                                      "^Organization:"
                                      "^X-Attribution:"
                                      "^\\(Posted\\|Date\\):")
      wl-summary-line-format "%T %P %W %D/%M %h:%m %[ %17f %]%[%1@%] %t%C%s")

(zenburn-with-color-variables
  (custom-set-faces
   `(wl-highlight-summary-displaying-face ((t (:weight normal))))
   `(wl-highlight-folder-path-face ((t (:foreground ,zenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,zenburn-blue-1 :weight bold))))
   `(wl-highlight-folder-few-face ((t (:foreground ,zenburn-blue-1 :weight bold))))
   `(wl-highlight-summary-new-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,zenburn-fg :weight normal))))))

(provide 'nemacs-email)
