;; |==============================================|
;; |  title: editor.el                            |
;; |  description: editor variables config        |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; variables
(defvar user-saves-directory (concat user-emacs-directory "saves/"))

;; editor variables
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; backup variables
(setq backup-by-copying t)
(setq backup-directory-alist '((user-saves-directory))
      version-control 'numbered
      make-backup-files t
      delete-old-versions 'never)

;; visual variables
(set-face-attribute 'default nil :height 150)
(set-cursor-color "#cccccc")
(set-face-foreground 'highlight nil)
(set-face-underline-p 'highlight nil)

(provide 'editor)
