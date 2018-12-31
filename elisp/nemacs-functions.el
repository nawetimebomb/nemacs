;;; nemacs-functions.el --- My own functions.

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

(defun nemacs-open-jira-ticket ()
  "Open a JIRA ticket in the default browser."
  (interactive)
  (browse-url (concat "https://jira.itx.com/browse/" (read-string "Enter a JIRA Ticket: "))))
(global-set-key (kbd "C-c j") #'nemacs-open-jira-ticket)

(defun nemacs-startup ()
  "Open my work agenda"
  (interactive)
  (require 'org)
  (org-agenda :keys "go"))
(global-set-key (kbd "C-c d") #'nemacs-startup)

(provide 'nemacs-functions)
