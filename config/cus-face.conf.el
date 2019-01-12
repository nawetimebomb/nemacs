(use-package cus-face
  :ensure nil
  :custom-face
  (default ((t (:background "WhiteSmoke"
                            :box nil
                            :family "Envy Code R 14"
                            :foreground "gray19"
                            :foundry "ENVY"
                            :height unspecified
                            :inherit nil
                            :inverse-video nil
                            :overline nil
                            :slant normal
                            :stipple nil
                            :strike-through nil
                            :underline nil
                            :weight normal
                            :width normal))))
  (cursor ((t (:background "DarkSlateBlue"))))
  (fixed-pitch ((t (:family "Monospace"))))
  (variable-pitch ((((type w32)) (:foundry "outline"
                                           :family "Arial"))
                   (t (:family "DejaVu Sans Mono"))))
  (escape-glyph ((t (:foreground "#008ED1"))))
  (homoglyph ((t (:foreground "#008ED1"))))
  (minibuffer-prompt ((t (:background "WhiteSmoke"
                                      :foreground "grey19"
                                      :weight bold))))
  (highlight ((t (:underline nil
                             :background "gold"))))
  (hl-line ((t (:background "gainsboro"))))
  (region ((t (:background "navy"
                           :foreground "WhiteSmoke"))))
  (shadow ((t (:foreground "gray50"))))
  (secondary-selection ((t (:background "yellow"
                                        :weight bold))))
  (trailing-whitespace ((t (:background "red"))))
  (font-lock-builtin-face ((t (:foreground "BlueViolet"))))
  (font-lock-comment-delimiter-face ((t (:foreground "DarkSeaGreen4"))))
  (font-lock-comment-face ((t (:foreground "DarkSeaGreen4"
                                           :slant unspecified))))
  (font-lock-constant-face ((t (:foreground "red"
                                            :slant italic
                                            :weight normal))))
  (font-lock-doc-face ((t (:foreground "DarkSeaGreen4"))))
  (font-lock-function-name-face ((t (:foreground "gray19"
                                                 :weight bold))))
  (font-lock-keyword-face ((t (:foreground "ForestGreen"
                                           :weight bold))))
  (font-lock-negation-char-face ((t nil)))
  (font-lock-preprocessor-face ((t (:foreground "gray51"))))
  (font-lock-regexp-grouping-backslash ((t (:inherit nil
                                                     :weight bold))))
  (font-lock-regexp-grouping-construct ((t (:inherit nil
                                                     :weight bold))))
  (font-lock-string-face ((t (:foreground "DarkRed"))))
  (font-lock-type-face ((t (:foreground "MediumBlue"
                                        :weight bold))))
  (font-lock-variable-name-face ((t (:foreground "OrangeRed"
                                                 :weight bold))))
  (font-lock-warning-face ((t (:foreground "red"
                                           :weight bold))))
  (button ((t (:foreground "#1F00FF"
                           :underline (:color foreground-color :style line)))))
  (link ((t (:foreground "#1F00FF"
                         :underline (:color foreground-color :style line) :weight normal))))
  (link-visited ((t (:foreground "DarkOrchid"
                                 :underline (:color foreground-color :style line)))))
  (fringe ((t (:background "WhiteSmoke"
                           :foreground "LightSteelBlue"))))
  (header-line ((t (:background "WhiteSmoke"
                                :foreground "black"
                                :overline "black"
                                :underline (:color "black" :style line) :weight normal))))
  (tooltip ((t (:background "light yellow"
                            :foreground "black"))))
  (isearch ((t (:background "PaleGreen"
                            :foreground "black"
                            :underline nil
                            :weight bold))))
  (isearch-fail ((t (:background "IndianRed"
                                 :foreground "white"
                                 :strike-through t
                                 :weight bold))))
  (lazy-highlight ((t (:background "yellow"
                                   :underline nil))))
  (match ((t (:weight bold
                      :background "yellow"))))
  (next-error ((t (:background "yellow"))))
  (query-replace ((t (:inherit (isearch)))))
  (show-paren-match ((t (:background "gainsboro"))))
  (show-paren-mismatch ((t (:background "IndianRed"
                                        :foreground "white"))))
  (widget-button ((t (:foreground "#1F00FF"
                                  :underline t
                                  :weight normal)))))
