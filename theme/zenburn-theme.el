(deftheme zenburn
  "Zenburn theme, my favorite")

;; NOTE: This theme is not finished and will not ever be finished.
;; Seriously, I like to do changes all the time.

(custom-theme-set-faces
 'zenburn
 '(default ((t (:background "#242424" :box nil :foreground "#ababaa" :font "DejaVu Sans Mono"))))
 '(bold ((t (:background unspecified :foreground unspecified :weight bold))))
 '(cursor ((t (:background "#d0bf8f" :foreground unspecified))))
 '(italic ((t (:background unspecified :foreground unspecified :underline nil))))
 '(region ((t (:background "#003b81" :foreground "#f0f0f0"))))
 '(secondary-selection ((t (:background "#e1e1e1"))))

 '(trailing-whitespace ((t (:background "#d9a0a0"))))

 '(match ((t (:background "#003b81" :foreground "#f0f0f0"))))
 '(isearch ((t (:weight normal :foreground "#993030" :background "#cc9393"))))
 '(isearch-fail ((t (:background "#aa3030" :foreground "#c1c1c1" :weight bold))))
 '(lazy-highlight ((t (:inherit (match)))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))

 '(anzu-match-1 ((t (:inherit (match)))))
 '(anzu-match-2 ((t (:inherit (match)))))
 '(anzu-match-3 ((t (:inherit (match)))))
 '(anzu-mode-line ((t (:background unspecified :foreground "#101010" :weight bold))))

 ;; CODING
 '(font-lock-builtin-face ((t (:foreground "#f0f0f0" :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#f0f0f0" :weight normal))))
 '(font-lock-preprocessor-face ((t (:foreground "#f0f0f0" :weight normal))))

 '(font-lock-comment-face ((t (:foreground "#7f9f7f"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#7f9f7f"))))
 '(font-lock-warning-face ((t (:foreground "#7f9f7f" :weight bold))))
 '(font-lock-doc-face ((t (:foreground "#7f9f7f"))))

 '(font-lock-function-name-face ((t (:inherit (default)))))
 '(font-lock-variable-name-face ((t (:inherit (default)))))

 '(font-lock-constant-face ((t (:foreground "#003b81" :weight bold))))

 '(font-lock-type-face ((t (:foreground "#cc9393"))))

 '(font-lock-string-face ((t (:foreground "#cc9393"))))

 '(js2-external-variable ((t (:foreground unspecified :inherit (default)))))
 '(js2-function-param ((t (:foreground unspecified :inherit (default)))))
 '(js2-jsdoc-tag ((t (:foreground "#7f9f7f" :weight bold))))
 '(typescript-jsdoc-tag ((t (:foreground "#7f9f7f" :weight bold))))
 '(js2-jsdoc-type ((t (:foreground "#8cd0d3"))))
 '(typescript-jsdoc-type ((t (:foreground "#8cd0d3"))))
 '(js2-jsdoc-value ((t (:foreground "#f0dfaf"))))
 '(typescript-jsdoc-value ((t (:foreground "#f0dfaf"))))
 '(js2-error ((t (:foreground "#cc9393"))))

 '(show-paren-match-face ((t (:background "#7f9f7f" :foreground "#1f1f1f" :inherit (default)))))
 '(show-paren-mismatch-face ((t (:background "#cc9393" :foreground "#1f1f1f" :inherit (default)))))

 '(tide--hl-highlight ((t (:background "#7f9f7f"))))

 ;; ORG
 '(org-level-1 ((t (:foreground "#dc8cc3" :height 200 :weight bold))))
 '(org-level-2 ((t (:foreground "#7cb8bb" :height 150 :weight bold))))
 '(org-level-3 ((t (:foreground "#cc9393" :height 120 :weight bold))))
 '(org-level-4 ((t (:foreground "#f0dfaf" :weight bold))))
 '(org-hide ((t (:inherit (default) :inverse-video t))))
 '(org-warning ((t (:inherit (error)))))

 ;; UI
 '(company-scrollbar-bg ((t (:background "#878777"))))
 '(company-scrollbar-fg ((t (:background "#d0bf8b"))))
 '(company-tooltip ((t (:inherit (mode-line)))))
 '(company-tooltip-annotation ((t (:foreground "#93e0e3"))))
 '(company-tooltip-common ((t (:foreground "#e69f67"))))
 '(company-tooltip-selection ((t (:background "#878777" :foreground "#d0bf8f" :weight bold))))

 '(minibuffer-prompt ((t (:foreground "#7cb8bb" :weight bold))))

 '(mode-line ((t (:background "#2f4f2f" :box nil :foreground "#ababaa"))))
 '(mode-line-buffer-id ((t (:background unspecified :distant-foreground "#000000" :foreground "#ababaa" :weight bold))))
 '(mode-line-inactive ((t (:background "#878777" :foreground "#202020" :inherit (mode-line))))))
