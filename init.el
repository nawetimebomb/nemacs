;;; init.el --- NEMACS Initialization File.

;; Copyright (C) 2017 ~ 2022 Nahuel Jesús Sacchetti <me@nsacchetti.com>

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
;;; PACKAGES

;; Load `core' configuration
(load (concat user-emacs-directory "core/core.el"))

;; Load all files from `modules/'
(mapc 'load (file-expand-wildcards (concat user-emacs-directory "modules/*.el")))

;; Load all files from `programming/'
(mapc 'load (file-expand-wildcards (concat user-emacs-directory "programming/*.el")))

;; Load all files from `custom/'
(mapc 'load (file-expand-wildcards (concat user-emacs-directory "custom/*.el")))

(when NEMACS-OS
  (load (expand-file-name "os/org-roam.el" user-emacs-directory))
  (load (expand-file-name "os/mu4e.el" user-emacs-directory))
  (load (expand-file-name "os/vterm.el" user-emacs-directory))
  (load (expand-file-name "os/lastpass.el" user-emacs-directory))
  (load (expand-file-name "os/exwm.el" user-emacs-directory)))

;;
;;; FONT

(IS-LINUX
 (set-fontset-font t 'unicode (font-spec :name "Envy Code R-15") nil)
 (set-face-font 'default "Envy Code R-15"))

(IS-WINDOWS
 (set-fontset-font t 'unicode (font-spec :name "Envy Code R-16") nil)
 (set-face-font 'default "Envy Code R-16"))

;;
;;; INITIALIZE EMACS

(nemacs-initialize)
