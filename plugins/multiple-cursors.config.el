;; multiple-cursors configuration file

(require 'multiple-cursors)

;; select every match
(global-set-key (kbd "C-S-l") 'mc/mark-all-like-this)

(provide 'multiple-cursors.config)
