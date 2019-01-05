(use-package company
  :custom-face
  (company-tooltip ((t (:background "gainsboro"))))
  (company-tooltip-selection ((t (:inherit mode-line))))
  (company-preview-common ((t (:foreground "white"
                                           :weight bold))))
  (company-preview-search ((t (:foreground "white"
                                           :weight bold))))
  (company-tooltip-annotation ((t (:foreground "white"
                                               :weight bold))))
  (company-tooltip-annotation-selection ((t (:foreground "white"
                                                         :weight bold))))
  (company-tooltip-common ((t (:foreground "SeaGreen"
                                           :weight bold))))
  (company-tooltip-common-selection ((t (:foreground "white"
                                                     :weight bold))))
  (company-tooltip-mouse ((t (:inherit hl-line))))
  (company-tooltip-search ((t (:inherit lazy-highlight))))
  (company-tooltip-search-selection ((t (:inherit isearch)))))
