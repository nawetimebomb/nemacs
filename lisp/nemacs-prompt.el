;;; nemacs-prompt.el --- A prompt buffer similar to `org-capture'

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

(defun nemacs-create-prompt-buffer (table title &optional prompt)
  "Create a prompt buffer with the given `table', `title' and `prompt' message.
`table' parameter should be a list of tuplets (at least) with the `key' in car and the `description' in cdr.
`title' parameter is used as the header of this temporal buffer.
`prompt' parameter is the message that will appear in the minibuffer. Default is 'Select:'.
This function returns returns the item in the `table' list."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Nemacs Select*"))
          (prompt (or prompt "Select: "))
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g" "q"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate buffer
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert prefix "[" k "]" "..." "  " desc "..." "\n")))
                      ;; Usable Entry
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert prefix "[" k "]" "     " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Shows "q" "exit"
                (insert "\n------------------------------\n\n")
                (insert "[q]      Exit capture mode\n")
                ;; Display UI
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (nemacs--read-key allowed-keys prompt)))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ((equal pressed "q") (user-error "Abort"))
                   ((member pressed des-keys))
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry)))))))))
        (when buffer (kill-buffer buffer))))))

(defun nemacs--read-key (allowed-keys prompt)
  "Read the `prompt' in minibuffer and tries to match it with the `allowed-keys'.
If the pressed key is in `allowed-keys', returns the key.
If the pressed key is not in `allowed-keys', shows an error message and tries again."
  (let* ((key (char-to-string
               (pcase (read-char-exclusive prompt)
                 ((or ?\s ?\t ?\r) ?\t)
                 (char char)))))
    (if (member key allowed-keys)
        key
      (message "Invalid key: `%s'" key)
      (sit-for 1)
      (nemacs--read-key allowed-keys prompt))))
