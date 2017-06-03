;; |==============================================|
;; |  title: shortcuts.el                         |
;; |  description: shortcuts configuration        |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; revert buffer
(global-set-key (kbd "C-x M-r") 'vc-revert)

;; toggle Neotree sidebar
(global-set-key [f8] 'neotree-toggle)

;; show Neotree sidebar
(global-set-key [f9] 'neotree-show)

;; only for Mac
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta))

(provide 'shortcuts)
