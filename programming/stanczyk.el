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
    '("asm" "bind" "const" "else" "fn" "leave" "in" "done"
      "loop"
      "peek" "if" "fi" "until" "while" "ret" "var" "when"))

  (defconst stanczyk-special
    '("inline" "private" "using"))

  (defconst stanczyk-typenames
    '("any" "bool" "char" "int" "ptr" "str"))

  (defconst stanczyk-constants
    '("true"
      "false"
      "SK_DEBUG"
      "OS_LINUX"
      "OS_WINDOWS"
      "OS_MAC"))

  (defface stanczyk-submit-face
    '((t :inherit font-lock-warning-face :bold nil))
    "Return face")

  (defconst stanczyk-font-lock-defaults
    `((;; Strings and chars
       ("\"\\.\\*\\?\\|'[\\]*.'" 0 'font-lock-string-face)
       ;; Constants between []
       ("\\[.*\\]" 0 'font-lock-variable-name-face)

       ;; Types
       (,(stanczyk-keywords-rx stanczyk-typenames) 0 'font-lock-builtin-face)
       ("\\<\$[a-zA-Z]*"                           0 'font-lock-builtin-face)

       ;; Keywords
       (,(stanczyk-keywords-rx stanczyk-keywords)  0 'font-lock-keyword-face)
       ("\\<let\\*?"   0 'font-lock-keyword-face)
       ("[c]?\\!$"   0 'font-lock-keyword-face)
       ("[c]?\\@"   0 'font-lock-keyword-face)

       ;; Other
       (,(stanczyk-keywords-rx stanczyk-special)   0 'stanczyk-submit-face)
       (,(stanczyk-keywords-rx stanczyk-constants) 0 'font-lock-constant-face)))))

(define-derived-mode stanczyk-mode prog-mode "Stanczyk"
  :syntax-table stanczyk-mode-syntax-table

  (setq mode-name "Stanczyk")
  (setq font-lock-defaults stanczyk-font-lock-defaults)
  (setq-local tab-width stanczyk-tab-width)
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local electric-indent-inhibit t)
  (setq-local backward-delete-char-untabify-method 'hungry)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq-default default-buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)

  (local-set-key (kbd "TAB") 'tab-to-tab-stop)

  ;; TODO: add hooks
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sk\\'" . stanczyk-mode))

(provide 'stanczyk-mode)
