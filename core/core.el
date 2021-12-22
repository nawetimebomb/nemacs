;;; core/core.el --- NEMACS CORE Main File.

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

(require 'battery)

;;
;;; NEMACS

(defconst nemacs-version "0.10b"
  "Version of `NEMACS'.")

(defconst nemacs-emacs-dir (expand-file-name user-emacs-directory)
  "Literally, the Emacs user directory or `user-emacs-directory'.

This variable is set to be used securely across NEMACS without
the risk of changing the default value of
`user-emacs-directory'.")

(defconst nemacs-local-dir (concat nemacs-emacs-dir ".local/")
  "The local folder where most of NEMACS stuff lives.")

(defconst nemacs-cache-dir (concat nemacs-local-dir "cache/")
  "The folder where all the soft data is saved.

Cached folder can be deleted at any time to reset saved
information, this can be done manually or
using `(nemacs-delete-cache)'.")

(defconst nemacs-etc-dir (concat nemacs-local-dir "etc/")
  "The folder where the hard data is saved.

This folder contains specific configuration files created by
Emacs or specific packages. This data should not be deleted.")

;; Create folder on first initialization
(defvar nemacs-directories (list nemacs-local-dir
                                 nemacs-cache-dir
                                 nemacs-etc-dir
                                 (concat nemacs-local-dir "packages")
                                 (concat nemacs-local-dir "packages/elpa"))
  "NEMACS directories. This is used on the initial setup.")

(dolist (dir nemacs-directories)
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;
;;; SYSTEM VARIABLES

;; Define variables for system identification
(defconst EMACS27+  (> emacs-major-version 26))
(defconst IS-LINUX  (eq system-type 'gnu/linux))
(defconst IS-LAPTOP (and battery-status-function
                         (not (string-match-p "N/A"
                                              (battery-format "%B"
                                                              (funcall battery-status-function))))))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;;
;;; ENCODING

;; Set encoding to UTF-8 across the board.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; And fix the Windows clipboard by changing to UTF-8 since Windows
;; most likely is using a wider encoding (UTF-16)
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))

;;
;;; NEMACS CORE

;; Lockfiles and backup files
(setq create-lockfiles nil
      make-backup-files nil)
;; Custom file
(setq-default custom-file (expand-file-name "custom.el" nemacs-etc-dir))
;; Load all the core libraries.
(load (concat user-emacs-directory "core/editor.el"))
(load (concat user-emacs-directory "core/interface.el"))
(load (concat user-emacs-directory "core/packages.el"))

;; Save desktop before closing
(setq desktop-auto-save-timeout 30
      desktop-base-file-name "emacs.desktop"
      desktop-base-lock-name "emacs.lock"
      desktop-files-not-to-save "^$"
      desktop-load-locked-desktop nil
      desktop-path (list nemacs-cache-dir)
      desktop-save t
      desktop-dirname nemacs-cache-dir)
(desktop-save-mode 1)

;; Ask before quitting Emacs
(defun nemacs-prompt-before-exiting-emacs ()
  "Prompts before closing the frame with `C-x C-c'. Standarizes `emacs' and
`emacsclient'."
  (interactive)
  (if (y-or-n-p ">>> Quit Nemacs? ")
      (save-buffers-kill-terminal)
    (message "Good. You should never do it.")))

;; Remap keybindings
(global-set-key (kbd "C-x C-c") #'nemacs-prompt-before-exiting-emacs)

;; Do not suspend Emacs on C-z or C-x C-z
(define-key global-map (kbd "C-z") #'ignore)
(define-key global-map (kbd "C-x C-z") #'ignore)
;; Ask for `y-or-n' instead of `yes-or-no'.
(fset #'yes-or-no-p #'y-or-n-p)

;;
;;; RUN NEMACS

(defun nemacs-initialize ()
  "Initialize the NEMACS system."

  ;; Memory management before starting
  (eval-and-compile
    (setq gc-cons-threshold 402653184
          gc-cons-percentage 0.6))

  (setq max-lisp-eval-depth 50000
	    max-specpdl-size 13000)

  ;; Startup configuration
  (setq inhibit-default-init t
	    inhibit-startup-echo-area-message user-login-name
	    initial-major-mode 'fundamental-mode
	    initial-scratch-message nil
	    inhibit-startup-message t)

  ;; Add hoook to after-init
  (add-hook 'after-init-hook
            #'(lambda ()
		        (setq gc-cons-threshold 16777216
                      gc-cons-percentage 0.1)

		        (add-to-list 'default-frame-alist '(fullscreen . maximized)))))
