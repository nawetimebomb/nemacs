(zenburn-with-color-variables
  (custom-set-faces
   `(org-agenda-date ((t (:foreground ,zenburn-fg :height 1.1))))
   `(org-agenda-date-today ((t (:foreground ,zenburn-green+1 :underline t :inherit org-agenda-date))))
   `(org-agenda-date-weekend ((t (:foreground ,zenburn-bg+3 :inherit org-agenda-date))))
   `(org-agenda-structure ((t (:foreground ,zenburn-fg :weight bold :height 1.5))))
   `(org-ellipsis ((t (:height 0.8))))
   `(org-level-1 ((t (:foreground unspecified :weight normal))))
   `(org-level-2 ((t (:foreground ,zenburn-yellow-1))))
   `(org-priority ((t (:foreground ,zenburn-cyan))))
   `(org-property ((t (:foreground ,zenburn-green :weight bold))))
   `(org-property-value ((t (:foreground ,zenburn-green+4 :italic t))))
   `(org-special-keyword ((t (:foreground ,zenburn-green))))
   `(org-tag ((t (:foreground ,zenburn-blue))))
   `(org-todo ((t (:foreground ,zenburn-red-2 :underline nil :weight bold))))
   `(org-done ((t (:foreground ,zenburn-green-1 :underline t :weight bold)))))

  (setq org-todo-keyword-faces
        `(("TODO" . org-todo)
          ("WAITING" . (:foreground ,zenburn-blue :underline t :weight bold))
          ("DONE" . org-done)
          ("CANCELED" . (:foreground ,zenburn-red :underline t :weight bold)))))
