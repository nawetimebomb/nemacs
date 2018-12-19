;;; nemacs-keybindings.el --- My own keybindings.

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

;; Define new prefix key: `C-z'
(define-prefix-command 'ring-map)
(global-set-key (kbd "C-z") 'ring-map)

;;bbdb
(global-set-key (kbd "C-z b") #'bbdb)

;; erc
(global-set-key (kbd "C-z e") #'erc)

;; magit
(global-set-key (kbd "C-z g") #'magit-status)

;; mu4e
(global-set-key (kbd "C-z m") #'gnus)

(provide 'nemacs-global-keybindings)
