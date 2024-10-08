;;; programming/javascript.el --- NEMACS JavaScript Configuration File.

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
;;; NEMACS JAVASCRIPT

(use-package rjsx-mode
  :preface
  (defun nemacs-setup-rjsx-mode ()
    (flycheck-mode))
  :hook
  (rjsx-mode . nemacs-setup-rjsx-mode)
  :mode "\\.js\\'"
  :custom
  (js-indent-level 4)
  (sgml-basic-offset 4))
