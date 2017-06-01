;; |==============================================|
;; |  title: shortcuts.el                         |
;; |  description: shortcuts configuration        |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

; revert buffer
(global-set-key (kbd "C-x M-r") 'revert-buffer)

; toggle Neotree sidebar
(global-set-key [f8] 'neotree-toggle)

; show Neotree sidebar
(global-set-key [f9] 'neotree-show)

; open execute-extended-command with Helm
(global-set-key (kbd "M-x") 'helm-M-x)

; open find files with Helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)

; open buffers list with Helm
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
