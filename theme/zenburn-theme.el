(deftheme zenburn
  "Zenburn theme, my favorite")

;; NOTE: This theme is not finished and will not ever be finished.
;; Seriously, I like to do changes all the time.

(custom-theme-set-faces
 'zenburn
 '(default ((t (:background "#242424" :box nil :foreground "#ababaa" :font "DejaVu Sans Mono"))))
 '(bold ((t (:background unspecified :foreground unspecified :weight bold))))
 '(cursor ((t (:background "#8faf9f" :foreground unspecified))))
 '(italic ((t (:background unspecified :foreground unspecified :underline nil))))
 '(region ((t (:background "#003b81" :foreground "#f0f0f0"))))
 '(secondary-selection ((t (:background "#e1e1e1"))))

 '(whitespace-trailing ((t (:background "#aa3030"))))

 '(match ((t (:background "#003b81" :foreground "#f0f0f0"))))
 '(isearch ((t (:background "#cc9393" :foreground "#993030" :weight normal))))
 '(isearch-fail ((t (:background "#aa3030" :foreground "#c1c1c1" :weight bold))))
 '(lazy-highlight ((t (:inherit (match)))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))

 '(anzu-match-1 ((t (:inherit (match)))))
 '(anzu-match-2 ((t (:inherit (match)))))
 '(anzu-match-3 ((t (:inherit (match)))))
 '(anzu-mode-line ((t (:inherit (mode-line) :background unspecified :weight bold))))

 ;; CODING
 '(font-lock-builtin-face ((t (:foreground "#f0f0f0" :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#d0d0d0" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#f0f0f0" :weight normal))))
 '(font-lock-constant-face ((t (:foreground "#f0f0f0" :weight normal))))

 '(font-lock-comment-face ((t (:foreground "#7f9f7f"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#7f9f7f"))))
 '(font-lock-warning-face ((t (:foreground "#7f9f7f" :weight bold))))
 '(font-lock-doc-face ((t (:foreground "#7f9f7f"))))

 '(font-lock-function-name-face ((t (:inherit (default)))))
 '(font-lock-variable-name-face ((t (:inherit (default)))))

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
 '(org-level-1 ((t (:foreground "#dc8cc3" :weight bold))))
 '(org-level-2 ((t (:foreground "#7cb8bb" :weight bold))))
 '(org-level-3 ((t (:foreground "#cc9393" :weight bold))))
 '(org-level-4 ((t (:foreground "#f0dfaf" :weight normal))))
 '(org-hide ((t (:inherit (default) :inverse-video t))))
 '(org-warning ((t (:inherit (error)))))

 ;; UI
 '(company-scrollbar-bg ((t (:background "#2f4f2f"))))
 '(company-scrollbar-fg ((t (:background "#ababaa"))))
 '(company-tooltip ((t :background "#131313" (:inherit (mode-line)))))
 '(company-tooltip-annotation ((t (:foreground "#93e0e3"))))
 '(company-tooltip-common ((t (:foreground "#e69f67"))))
 '(company-tooltip-selection ((t (:inherit (mode-line) :weight bold))))

 '(minibuffer-prompt ((t (:foreground "#7cb8bb" :weight bold))))

 ;; This mode-line configuration is used by the spaceline package.
 '(mode-line ((t (:background "#2f4f2f" :box nil :foreground "#ababaa"))))
 '(powerline-active1 ((t (:background "#505050" :foreground "#121212"))))
 '(powerline-active2 ((t (:inherit (default)))))
 '(spaceline-highlight-face ((t (:inherit (mode-line)))))
 '(spaceline-modified ((t (:background "#aa3030" :inherit (mode-line)))))
 '(spaceline-unmodified ((t (:inherit (mode-line)))))
 '(spaceline-read-only ((t (:background "#003b81" :inherit (mode-line)))))
 '(mode-line-buffer-id ((t (:background unspecified :distant-foreground "#000000" :foreground "#ababaa" :weight bold))))
 '(mode-line-inactive ((t (:inherit (powerline-active1)))))
 '(powerline-inactive1 ((t (:inherit (powerline-active1)))))
 '(powerline-inactive2 ((t (:inherit (powerline-active1)))))

 '(neo-dir-link-face ((t :foreground "#7cb8bb" :weight bold)))
 '(neo-header-face ((t :inherit (default))))
 '(neo-root-dir-face ((t :inherit (default) :weight bold)))
 '(neo-vc-edited-face ((t (:foreground "#db7625" :weight bold))))
 '(neo-vc-up-to-date-face ((t (:inherit (default))))))
