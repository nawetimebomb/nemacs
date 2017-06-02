;; |==============================================|
;; |  title: editor.el                            |
;; |  description: editor variables config        |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; editor variables
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; backup variables
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/saves"))
      version-control 'numbered
      make-backup-files t
      delete-old-versions 'never)

;; visual variables
(set-face-attribute 'default nil :height 160)
(set-cursor-color "#ffb269")
(set-face-attribute 'highlight nil
                    :foreground "#151515")
(set-face-attribute 'region nil
                    :background "#ffb269")

(provide 'editor)
