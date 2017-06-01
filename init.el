;; |==============================================|
;; |  title: init.el                              |
;; |  description: emacs igniter!                 |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; global variables
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))

;; startup emacs
(defvar gc-cons-threshold--original gc-const-threshold)
(setq gc-const-threshold (* 100 1024 1024)) ; limit garbage collection before init

;; load plugin configurations


;; emacs is initialized
(run-with-idle-timer 5 nil (lambda () (setq gc-const-threshold gc-const-threshold--original))) ; reset garbage collection limit
