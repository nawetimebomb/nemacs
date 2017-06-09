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
      visible-bell nil)

;; backup variables
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/saves"))
      version-control 'numbered
      make-backup-files t
      delete-old-versions 'never)

;; visual variables
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :height 150)
  (set-face-attribute 'default nil :height 120))
(setq mac-allow-anti-aliasing t)
(set-cursor-color custom-editor-cursor-color)
(set-face-attribute 'highlight nil
                    :background custom-background-editor-highlight-color
                    :foreground custom-foreground-editor-highlight-color)
(set-face-attribute 'region nil
                    :background custom-background-editor-region-color
                    :foreground custom-foreground-editor-region-color)
(delete-selection-mode)

(provide 'editor)
