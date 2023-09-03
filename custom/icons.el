;;; custom/icons.el --- NEMACS CUSTOM Icons Configuration File.

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
;;; NEMACS ICONS

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-color-icons nil))

(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode))
