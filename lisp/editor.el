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
(set-face-attribute 'default nil :height 170)
(set-cursor-color custom-editor-cursor-color)
(set-face-attribute 'highlight nil
                    :background custom-background-editor-highlight-color
                    :foreground custom-foreground-editor-highlight-color)
(set-face-attribute 'region nil
                    :background custom-background-editor-region-color
                    :foreground custom-foreground-editor-region-color)

(provide 'editor)
