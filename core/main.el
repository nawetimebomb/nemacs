;;; core/core.el --- NEMACS CORE Main File.

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

(require 'battery)
(require 'desktop)

;;
;;; NEMACS

(defconst nemacs-version "0.20"
  "Version of `NEMACS'.")

(defconst nemacs-local-dir (concat user-emacs-directory ".local/")
  "The local folder where most of NEMACS stuff lives.")

(defconst nemacs-cache-dir (concat nemacs-local-dir "cache/")
  "The folder where all the soft data is saved.

Cached folder can be deleted at any time to reset saved
information, this can be done manually.")

(defconst nemacs-tmp-dir (concat nemacs-local-dir "tmp/")
  "The folder where temporary files for NEMACS are created

This folder contains temporary data that can be deleted whenever.
The content of this folder should be cleaned up on `C-x C-c'.")

;; Create folder on first initialization
(defvar nemacs-directories (list nemacs-local-dir
                                 nemacs-cache-dir
                                 nemacs-tmp-dir)
  "NEMACS directories. This is used on the initial setup.")

(dolist (dir nemacs-directories)
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defcustom NEMACS-OS nil
  "Enables the NEMACS-OS. The Operating System contains
EXWM as the Window Manager, Org Roam as note taking tool and
more.

Change this value on a `user-emacs-directory/custom' file.")

(defcustom nemacs-widescreen nil
  "Enables widescreen options for NEMACS but needs to be enabled manually.

Change this value on a `user-emacs-directory/custom' file.")

;;
;;; SYSTEM VARIABLES

;; Define variables for system identification
;;;###autoload
(defmacro IS-EMACS27+ (&rest body)
  `(when (> emacs-major-version 26)
     ,@body))

;;;###autoload
(defmacro IS-LAPTOP (&rest body)
  `(when (and battery-status-function
              (not
               (string-match-p
                "N/A"
                (battery-format
                 "%B"
                 (funcall battery-status-function)))))
     ,@body))

;;;###autoload
(defmacro IS-LINUX (&rest body)
  `(when (eq system-type 'gnu/linux)
     ,@body))

;;;###autoload
(defmacro IS-WINDOWS (&rest body)
  `(when (memq system-type '(cygwin windows-nt ms-dos))
     ,@body))

;;
;;; ENCODING

;; Set encoding to UTF-8 across the board.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; And fix the Windows clipboard by changing to UTF-8 since Windows
;; most likely is using a wider encoding (UTF-16)
(IS-WINDOWS (setq selection-coding-system 'utf-8))

;;
;;; NEMACS CORE

;; Lockfiles and backup files
(setq create-lockfiles nil
      make-backup-files nil)
;; Custom file
(setq-default custom-file (make-temp-file (expand-file-name "emacs-custom-" nemacs-tmp-dir)))
;; Transient and Server files
(setq-default transient-history-file (expand-file-name "transient" nemacs-cache-dir))
(setq-default server-socket-dir nemacs-cache-dir)
;; Load all the core libraries.
(load (concat user-emacs-directory "core/editor.el"))
(load (concat user-emacs-directory "core/interface.el"))
(load (concat user-emacs-directory "core/straight.el"))

;; Save bookmarks on change
(setq bookmark-save-flag 1)

;; Default saving folders
(setq abbrev-file-name (concat nemacs-local-dir "abbrev.el")
      auto-save-list-file-name (concat nemacs-cache-dir "autosave")
      bookmark-default-file (concat nemacs-cache-dir   "bookmarks")
      nsm-settings-file (expand-file-name "ns.data" nemacs-cache-dir)
      pcache-directory (concat nemacs-cache-dir "pcache")
      recentf-save-file (expand-file-name "recentf" nemacs-cache-dir)
      savehist-file (expand-file-name "history" nemacs-cache-dir)
      url-history-file (expand-file-name "url.el" nemacs-cache-dir))

;; Save desktop before closing
(setq desktop-auto-save-timeout 30
      desktop-base-file-name "emacs.desktop"
      desktop-base-lock-name "emacs.lock"
      desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\'\\)"
      desktop-modes-not-to-save '(dired-mode
                                  org-mode
                                  rjsx-mode
                                  special-mode
                                  tags-table-mode
                                  vc-dir-mode)
      desktop-load-locked-desktop nil
      desktop-path (list nemacs-cache-dir)
      desktop-save nil
      desktop-dirname nemacs-cache-dir)
;; NOTE 1/7/22: I'm disabling the `desktop-save-mode' since using `recentf-mode' is simpler and cleaner.
(desktop-save-mode -1)

;; Disable ido-mode
(ido-mode -1)

;; Enable recentf-mode (Recent File)
(recentf-mode 1)

;; Session files
(eval-after-load 'x-win
  (let ((session-dir (concat nemacs-cache-dir "sessions/")))
        `(progn
           (make-directory ,session-dir t)
           (defun emacs-session-filename (session-id)
             (expand-file-name session-id ,session-dir)))))

;; Ask before quitting Emacs
(defun nemacs-prompt-before-exiting-emacs ()
  "Prompts before closing the frame with `C-x C-c'. Standarizes `emacs' and
`emacsclient'."
  (interactive)
  (let* ((tmp-files (directory-files nemacs-tmp-dir t "[a-z|A-Z]")))
  (if (y-or-n-p ">>> Quit Nemacs? ")
      (progn
        (dolist (tmp-file tmp-files)
          (delete-file tmp-file))
        (save-buffers-kill-terminal))
    (message "Good. You should never do it."))))

;; Remap keybindings
(global-set-key (kbd "C-x C-c") #'nemacs-prompt-before-exiting-emacs)

;; Do not suspend Emacs on C-z or C-x C-z
(define-key global-map (kbd "C-z") #'ignore)
(define-key global-map (kbd "C-x C-z") #'ignore)
;; Ask for `y-or-n' instead of `yes-or-no'.
(fset #'yes-or-no-p #'y-or-n-p)

;;
;;; RUN NEMACS

;;;###autoload
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
        inhibit-startup-message t
	    initial-major-mode 'fundamental-mode
	    initial-scratch-message nil)

  ;; Add hoook to after-init
  (add-hook 'after-init-hook
            #'(lambda ()
		        (setq gc-cons-threshold 16777216
                      gc-cons-percentage 0.1
                      read-process-output-max (* 1024 1024)))))
