;;; nemacs-programming.el --- The main file with programming documentation.

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

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|NOTE\\|TODO\\|BUG\\)"
                                       1 'font-lock-warning-face prepend)))))

(load (expand-file-name "js-mode.conf.el" nemacs-prog-mode-dir))
(load (expand-file-name "scss-mode.conf.el" nemacs-prog-mode-dir))
(load (expand-file-name "sgml-mode.conf.el" nemacs-prog-mode-dir))

(add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))
(add-to-list 'auto-mode-alist `(,(rx ".less" string-end) . scss-mode))
(add-to-list 'auto-mode-alist `(,(rx ".sass" string-end) . scss-mode))
(add-to-list 'auto-mode-alist `(,(rx ".scss" string-end) . scss-mode))


(provide 'nemacs-programming)
