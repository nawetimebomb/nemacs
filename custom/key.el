;;; custom/key.el --- NEMACS CUSTOM Key Configuration File.

;; Copyright (C) 2017 ~ 2023 Nahuel Jesús Sacchetti <nemacs@nsacchetti.com>

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
;;; NEMACS KEY

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-popup-type 'minibuffer)
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 0.2)
  (which-key-idle-secondary-delay nil))
