;; |==============================================|
;; |  title: init.el                              |
;; |  description: emacs igniter!                 |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; ignite!
;;(package-initialize)

(setq package-enable-at-startup nil)

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

;; On startup
(defvar gc-cons-threshold--original gc-cons-threshold)
(setq gc-const-threshold (* 100 1024 1024)) ; limit garbage collection before init (100 MB)
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(require 'setup-packages)

;; load plugin configurations
(require 'use-package.config)
(require 'helm.config)
(require 'spaceline.config)
(require 'git-gutter.config)
(require 'multiple-cursors.config)
(require 'find-file-in-project.config)
(require 'dumb-jump.config)

;; load local configurations
(require 'hooks)
(require 'modes) ; refactor this to plugin configuration
(require 'shortcuts)
(require 'themes)
(require 'editor)

;; on finish
(run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold gc-cons-threshold--original))) ; reset garbage collection limit
