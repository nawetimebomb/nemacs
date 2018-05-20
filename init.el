;;; Nemacs

;;; Commentary:
;; The entry point into Nemacs.

;;; Code:

(advice-add #'display-startup-echo-area-message :override #'ignore)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      mode-line-format nil)

(let ((file-name-handler-alist nil))
  (if (file-exists-p (expand-file-name "nemacs.elc" user-emacs-directory))
      (load-file (expand-file-name "nemacs.elc" user-emacs-directory))
    (require 'org)
    (org-babel-load-file (expand-file-name "nemacs.org" user-emacs-directory))))
