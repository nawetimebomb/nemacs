;;; programming/stanczyk.el --- NEMACS Stanczyk Configuration File.

;; Copyright (C) 2017 ~ 2024 Nahuel Jesús Sacchetti <me@nsacchetti.com>

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;
;;; NEMACS STANCZYK

(defvar stanczyk-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; - and _ are word constituents
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)

    (modify-syntax-entry ?\" "\"" st)

	(modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st) st))

(eval-and-compile
  (defconst stanczyk-tab-width 2 "Tab width for Stanczyk mode")

  (defun stanczyk-indent-two-spaces ()
    (insert "  "))

  (defun stanczyk-wrap-reserved-rx (s)
    (concat "\\<" s "\\>"))

  (defun stanczyk-keywords-rx (keywords)
    (stanczyk-wrap-reserved-rx (regexp-opt keywords t)))

  (defconst stanczyk-keywords
    '("extern" "bind" "const" "else" "fn" "if"
      "loop" "reserve" "ret"))

  (defconst stanczyk-special
    '("using"))

  (defconst stanczyk-typenames
    '("bool" "int" "ptr" "str"))

  (defconst stanczyk-constants
    '("true" "false"))

  (defface stanczyk-submit-face
    '((t :inherit font-lock-warning-face :bold nil))
    "Return face")

  (defconst stanczyk-font-lock-defaults
    `((;; Strings and chars
       ("\"\\.\\*\\?\\|'[\\]*.'"                   0 'font-lock-string-face)

       ;; Types
       (,(stanczyk-keywords-rx stanczyk-typenames) 0 'font-lock-type-face)

       ;; Keywords
       (,(stanczyk-keywords-rx stanczyk-keywords)  0 'font-lock-keyword-face)

       ;; Other
       (,(stanczyk-keywords-rx stanczyk-special)   0 'stanczyk-submit-face)
       (,(stanczyk-keywords-rx stanczyk-constants) 0 'font-lock-constant-face)))))

(define-derived-mode stanczyk-mode prog-mode "Stanczyk"
  :syntax-table stanczyk-mode-syntax-table

  (setq mode-name "Stanczyk")
  (setq font-lock-defaults stanczyk-font-lock-defaults)
  (setq-local tab-width stanczyk-tab-width)
  (setq-local indent-tabs-mode t)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local electric-indent-inhibit t)
  (setq-local backward-delete-char-untabify-method 'hungry)

  (local-set-key (kbd "TAB") 'tab-to-tab-stop)

  ;; TODO: add hooks
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sk\\'" . stanczyk-mode))

(provide 'stanczyk-mode)
