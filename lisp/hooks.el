;; |==============================================|
;; |  title: hooks.el                             |
;; |  description: hooks configuration            |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; delete unnecesary tabs before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'hooks)
