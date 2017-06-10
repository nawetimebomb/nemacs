;; |==============================================|
;; |  title: editor.el                            |
;; |  description: editor variables config        |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; editor variables
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq confirm-kill-emacs 'yes-or-no-p
      visible-bell 'bottom
      display-time-format "%H:%M")
(display-time-mode)
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit
                                      exit-minibuffer
                                      keyboard-quit))
          (ding)
          (call-process-shell-command "xset led named 'Scroll Lock'")
          (run-with-idle-timer 0.5 nil (lambda () (call-process-shell-command "xset -led named 'Scroll Lock'"))))))

;; backup variables
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/saves"))
      version-control 'numbered
      make-backup-files t
      delete-old-versions 'never)

(provide 'editor)
