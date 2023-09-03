;;; custom/file.el --- NEMACS CUSTOM File Configuration File.

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
;;; NEMACS FILE

(use-package consult
  :bind
  (("M-g g" . consult-goto-line)
   ("M-y"   . consult-yank-from-kill-ring)
   ("C-x b" . consult-buffer)
   ("C-x B" . consult-buffer-other-window)))

(use-package marginalia
  :bind
  ((:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package project
  :custom
  (project-list-file (expand-file-name "projects" nemacs-cache-dir)))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-scroll-margin 0))
