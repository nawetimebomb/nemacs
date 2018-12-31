(global-set-key [remap query-replace] #'anzu-query-replace)
(global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp)

(custom-set-faces
 '(anzu-mode-line
   ((t (:inherit mode-line))))
 '(anzu-mode-line-no-match
   ((t (:foreground "white" :inherit mode-line :strike-through t)))))
