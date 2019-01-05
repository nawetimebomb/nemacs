(use-package anzu
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :custom-face
  (anzu-mode-line ((t (:inherit mode-line))))
  (anzu-mode-line-no-match ((t (:foreground "white" :inherit mode-line :strike-through t)))))
