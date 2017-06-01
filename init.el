;; |==============================================|
;; |  title: init.el                              |
;; |  description: emacs igniter!                 |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; ignite!
(package-initialize)

;; global variables
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))
(defvar global-setup-directory (concat user-emacs-directory "lisp/"))
(defvar plugin-setup-directory (concat user-emacs-directory "plugins/"))
(defvar emacs-version-short (format "%s_%s"
                                    emacs-major-version emacs-minor-version))

;; add paths to load
(add-to-list 'load-path global-setup-directory)
(add-to-list 'load-path plugin-setup-directory)

;; on startup
(defvar gc-cons-threshold--original gc-cons-threshold)
(setq gc-const-threshold (* 100 1024 1024)) ; limit garbage collection before init (100 MB)
(require 'setup-packages)

;; load plugin configurations
(require 'helm.config)
(require 'powerline.config)

;; load local configurations
(require 'hooks)
(require 'modes)
(require 'shortcuts)
(require 'themes)
(require 'editor)

;; on finish
(run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold gc-cons-threshold--original))) ; reset garbage collection limit
