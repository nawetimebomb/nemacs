;;; init.el --- Nemacs Initialization File.

;; Copyright (C) 2017 ~ 2019 Nahuel Jesús Sacchetti <nahueljsacchetti@gmail.com>

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

;; Ignore startup messages in the echo area
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Define important variables
(eval-and-compile
  (defvar nemacs-shared-dir "~/Dropbox/"
    "My shared folder on the Cloud")

  (defvar nemacs-emacs-dir (expand-file-name user-emacs-directory)
    "The path of .emacs.d.")

  (defvar nemacs-local-dir (concat nemacs-emacs-dir ".local/")
    "Root directory for my local Emacs files.")

  (defvar nemacs-config-dir (concat nemacs-emacs-dir "config/")
    "Directory with the configuration files.")

  (defvar nemacs-config-file-list '()
    "List of NEMACS configuration files.")

  (defvar nemacs-etc-dir (concat nemacs-local-dir "etc/")
    "Local directory for non-volatile storage. They ussually are not deleted. Use this for dependencies like servers or config files.")

  (defvar nemacs-cache-dir (concat nemacs-local-dir "cache/")
    "Local directory for volatile storage.")

  (defvar nemacs-packages-dir (concat nemacs-local-dir "packages/")
    "Where `package.el' and my local plugins are installed.")

  (defvar nemacs-elisp-dir (concat nemacs-emacs-dir "elisp/")
    "Directory with NEMACS's interesting code.")

  (defvar nemacs-private-elisp-dir (concat nemacs-shared-dir "elisp/")
    "Directory with elisp code with sensible information.")

  (defvar nemacs-themes-dir (concat nemacs-emacs-dir "themes/")
    "The custom themes directory.")

  (defvar nemacs-notes-dir (concat nemacs-shared-dir "orgfiles/")
    "Notes directory where all the shared org files are stored.")

  (defvar nemacs-enable-extras t
    "When non-nil enables the Nemacs extras (like more packages and modes, synchronization and more).")

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
              fill-column 80
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
              use-package-always-ensure t
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

(when window-system
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(cd "~/")
(fset #'yes-or-no-p #'y-or-n-p)
(show-paren-mode t)
(global-auto-revert-mode t)
(global-subword-mode t)
(delete-selection-mode t)
(column-number-mode t)
(set-fontset-font t 'unicode (font-spec :name "DejaVu Sans Mono") nil)
(set-face-font 'default "Envy Code R 14")

;; Initialization
(add-to-list 'load-path nemacs-elisp-dir)
(add-to-list 'load-path nemacs-private-elisp-dir)

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

(setq package-archives '(("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package delight)
(use-package use-package-ensure-system-package)

;; Create a list of configuration files
(dolist (file (directory-files nemacs-config-dir))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (add-to-list 'nemacs-config-file-list (expand-file-name file nemacs-config-dir) t)))

(add-hook 'after-init-hook
          #'(lambda ()
              ;; Load configuration files
              (mapc (lambda (file)
                      (load file))
                    nemacs-config-file-list)

              ;; Reset defaults
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))
