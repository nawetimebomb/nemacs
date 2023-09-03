;;; programming/c.el --- NEMACS C Configuration File.

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
;;; NEMACS C

(use-package cc-mode
  :preface
  (defun nemacs-setup-cc-mode ()
    (setq flycheck-clang-language-standard "gnu99")
    (flycheck-mode)
    (c-set-offset 'case-label 4)
    (c-toggle-comment-style -1))

  (defun nemacs-compile-sources ()
    (interactive)
    (async-shell-command (concat (project-root (project-current)) "build.sh -norun") nil nil))
  :hook
  (c-mode . nemacs-setup-cc-mode)
  :bind
  ("<f10>"  . nemacs-compile-sources)
  :custom
  (c-basic-offset 4))
