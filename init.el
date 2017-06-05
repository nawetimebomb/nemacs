;; |==============================================|
;; |  title: init.el                              |
;; |  description: emacs igniter!                 |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; ignite!
(setq package-enable-at-startup nil)
;;(package-initialize)

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

;; install use-package
(require 'setup-packages)

(require 'color-configuration)

;; load plugin configurations
(require 'auto-compile.config)
(require 'column-marker.config)
(require 'dumb-jump.config)
(require 'find-file-in-project.config)
(require 'git-gutter.config)
(require 'helm.config)
(require 'linum-mode.config)
(require 'major-modes.config)
(require 'multiple-cursors.config)
(require 'smartparens.config)
(require 'spaceline.config)

;; load local configurations
(require 'hooks)
(require 'modes) ; refactor this to plugin configuration
(require 'shortcuts)
(require 'themes)
(require 'editor)

;; on finish
(run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold gc-cons-threshold--original))) ; reset garbage collection limit
