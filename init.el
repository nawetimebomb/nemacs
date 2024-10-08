;;; init.el --- NEMACS Initialization File.

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
;;; SETUP

;; Load `core' configuration
(load (concat user-emacs-directory "core/main.el"))

;; Load all files from `custom/'
(mapc 'load (file-expand-wildcards (concat user-emacs-directory "custom/*.el")))

;; Load all files from `programming/'
(mapc 'load (file-expand-wildcards (concat user-emacs-directory "programming/*.el")))

;; Load all files from `machine-specific/'
(mapc 'load (file-expand-wildcards (concat user-emacs-directory "machine-specific/*.el")))

;;
;;; FONT

(IS-LINUX
 (set-fontset-font t 'unicode (font-spec :name "Hack-15") nil)
 (set-face-font 'default "Hack-15"))

(IS-WINDOWS
 (set-fontset-font t 'unicode (font-spec :name "Hack-18") nil)
 (set-face-font 'default "Hack-18"))

;;
;;; INITIALIZE EMACS

(nemacs-initialize)
