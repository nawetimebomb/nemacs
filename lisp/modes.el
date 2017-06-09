;; |==============================================|
;; |  title: modes.el                             |
;; |  description: modes toggler configuration    |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; global modes
(global-auto-revert-mode t)
(global-hl-line-mode t)

;; buffer modes
(display-time-mode)
(setq display-time-format "%H:%M"
      display-time-default-load-average nil)
(which-function-mode t)
(column-number-mode 1)
(desktop-save-mode 1)

(provide 'modes)
