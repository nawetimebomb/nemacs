;;; init.el --- Nemacs initialization file.

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

;; This is a workaround for Emacs to initialize the packages automatically.
;; I don't have this issue with Emacs v27, prolly updating would be better.
;;(package-initialize)

;; Each file named <somelibrary>.conf.el is loaded just after the library is
;; loaded.
;; Code from Julien Danjou: https://github.com/jd/emacs.d
(dolist (file (directory-files user-emacs-directory))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (eval-after-load (match-string-no-properties 1 file)
      `(load ,(concat user-emacs-directory file)))))

;; Ignore startup messages in the echo area
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Define important variables
(eval-and-compile
  (defvar nemacs-emacs-dir (expand-file-name user-emacs-directory)
    "The path of .emacs.d.")

  (defvar nemacs-local-dir (concat nemacs-emacs-dir ".local/")
    "Root directory for my local Emacs files.")

  (defvar nemacs-preload-dir (concat nemacs-emacs-dir "preload/")
    "Directory with ELisp files that are meant to be preloaded, even if the module isn't loaded yet")

  (defvar nemacs-etc-dir (concat nemacs-local-dir "etc/")
    "Local directory for non-volatile storage. They ussually are not deleted. Use this for dependencies like servers or config files.")

  (defvar nemacs-cache-dir (concat nemacs-local-dir "cache/")
    "Local directory for volatile storage.")

  (defvar nemacs-packages-dir (concat nemacs-local-dir "packages/")
    "Where `package.el' and my local plugins are installed.")

  (defvar nemacs-lisp-dir (concat nemacs-emacs-dir "lisp/")
    "Directory with NEMACS's interesting code.")

  (defvar nemacs-themes-dir (concat nemacs-emacs-dir "themes/")
    "The custom themes directory.")

  (defvar nemacs-notes-dir "~/Dropbox/Notes"
    "Notes directory where all the shared org files are stored.")

  (dolist (dir (list
                nemacs-local-dir
                nemacs-etc-dir
                nemacs-cache-dir
                (expand-file-name "elpa" nemacs-packages-dir)
                nemacs-notes-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))

;; Default Settings
(setq-default auto-save-default nil
              bidi-display-reordering nil
              blink-matching-paren nil
              buffer-file-coding-system  'utf-8
              cursor-in-non-selected-windows nil
              custom-file (expand-file-name "custom.el" nemacs-etc-dir)
              create-lockfiles nil
              delete-by-moving-to-trash t
              display-time-format "%H:%M"
              fill-column 78
              frame-inhibit-implied-resize t
              frame-title-format "NEMACS"
              help-window-select t
              highlight-nonselected-windows nil
              fringe-indicator-alist (delq
                                      (assq 'continuation fringe-indicator-alist)
                                      fringe-indicator-alist)
              indent-tabs-mode nil
              indicate-buffer-boundaries nil
              indicate-empty-lines nil
              make-backup-files nil
              max-mini-window-height 0.3
              mode-line-default-help-echo nil
              mouse-yank-at-point t
              ns-right-alternate-modifier nil
              require-final-newline t
              resize-mini-windows 'grow-only
              ring-bell-function #'ignore
              show-help-function nil
              split-height-threshold nil
              split-width-threshold 160
              tab-always-indent t
              tab-width 4
              tabify-regexp "^\t* [ \t]+"
              truncate-lines nil
              uniquify-buffer-name-style 'post-forward-angle-brackets
              use-dialog-box nil
              vc-handled-backends nil
              visible-bell nil
              visible-cursor nil
              whitespace-line-column fill-column
              whitespace-style '(face tab trailing)
              word-wrap t
              x-stretch-cursor t)

(setq-default auto-save-list-file-name  (concat nemacs-cache-dir "autosave")
              bookmark-default-file     (concat nemacs-etc-dir "bookmarks")
              abbrev-file-name          (concat nemacs-local-dir "abbrev.el")
              pcache-directory          (concat nemacs-cache-dir "pcache")
              recentf-save-file         (expand-file-name "recentf" nemacs-cache-dir))

(fset #'yes-or-no-p #'y-or-n-p)
(tooltip-mode -1)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(show-paren-mode t)
(global-auto-revert-mode t)
(global-subword-mode t)
(delete-selection-mode t)
(column-number-mode t)
(flyspell-mode t)
(when (file-exists-p custom-file) (load-file custom-file))
(set-frame-font "Envy Code R 12")

;; Initialization
(add-to-list 'load-path nemacs-lisp-dir)

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)
  (setq package-user-dir (expand-file-name "elpa" nemacs-packages-dir)))

(setq max-lisp-eval-depth 50000
      max-specpdl-size 10000)

(setq load-prefer-newer noninteractive
      package-enable-at-startup nil)

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(require 'package)

;; Startup Hooks
(require 'nemacs-startup-hooks)

(add-hook 'after-init-hook
          #'(lambda ()
              ;; Reset defaults
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)

              ;; Mode line
              (setq-default mode-line-format
                            '("%e"
                              mode-line-front-space
                              mode-line-client
                              mode-line-modified
                              " "
                              mode-line-directory
                              mode-line-buffer-identification
                              " "
                              mode-line-position
                              (flycheck-mode flycheck-mode-line)
                              " "
                              mode-line-modes
                              mode-line-misc-info
                              mode-line-end-spaces))

              ;; Nemacs Lisp
              (require 'nemacs-email)
              (require 'nemacs-functions)
              (require 'nemacs-global-keybindings)
              (require 'nemacs-programming)

              ;; Packages Settings
              (helm-mode)
              (global-anzu-mode)
              (projectile-mode)
              (bbdb-initialize 'message)
              (bbdb-mua-auto-update-init 'message)

              ;; Run the startup page
              (nemacs-startup)))
