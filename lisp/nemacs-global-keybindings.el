;;; nemacs-keybindings.el --- My own keybindings. Only use *native Emacs functions*.

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

;; elfeed
(global-set-key (kbd "C-x w") #'elfeed)

;; gnus
(global-set-key (kbd "C-x g") #'gnus)

;; notmuch
(global-set-key (kbd "C-x m") #'notmuch)
(global-set-key (kbd "C-x h") #'helm-notmuch)
(global-set-key (kbd "C-x M-m") #'notmuch-mua-new-mail)


(provide 'nemacs-global-keybindings)
