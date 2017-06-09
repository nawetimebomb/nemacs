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
(defvar major-modes-setup-directory (concat user-emacs-directory "modes/"))
(defvar emacs-version-short (format "%s_%s"
                                    emacs-major-version emacs-minor-version))

;; add paths to load
(add-to-list 'load-path global-setup-directory)
(add-to-list 'load-path plugin-setup-directory)
(add-to-list 'load-path major-modes-setup-directory)

;; On startup
(defvar gc-cons-threshold--original gc-cons-threshold)
(setq gc-const-threshold (* 100 1024 1024)) ; limit garbage collection before init (100 MB)
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))

;; install use-package
(require 'use-package.config)
(require 'color-configuration)
(require 'themes)

;; load major-mode configurations from /modes
(require 'csharp-mode.config)
(require 'emacs-lisp-mode.config)
(require 'js2-mode.config)
(require 'json-mode.config)
(require 'org.config)
(require 'rjsx-mode.config)
(require 'web-mode.config)

;; load plugin configurations from /plugins
(require 'anzu.config)
(require 'better-defaults.config)
(require 'company.config)
(require 'disable-mouse.config)
(require 'emmet-mode.config)
(require 'dumb-jump.config)
(require 'git-gutter.config)
(require 'helm.config)
(require 'linum.config)
(require 'magit.config)
(require 'multiple-cursors.config)
(require 'neotree.config)
(require 'projectile.config)
(require 'spaceline.config)
(require 'smartparens.config)

;; load local configurations // refactor these to create single plugins for each.
(require 'hooks)
(require 'modes) ; refactor this to plugin configuration
(require 'shortcuts)
(require 'editor)

;; on finish
(run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold gc-cons-threshold--original))) ; reset garbage collection limit
