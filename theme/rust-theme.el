(deftheme rust
  "The Rust official colors inspired by The Rust Programming Language book")

;; NOTE: This theme is not finished and will not ever be finished.
;; Seriously, I like to do changes all the time.

(custom-theme-set-faces
 'rust
 '(default ((t (:background "#c1c1c1" :box nil :foreground "#161615" :font "DejaVu Sans Mono"))))
 '(bold ((t (:background unspecified :foreground unspecified :weight bold))))
 '(cursor ((t (:background "#b6111e" :foreground unspecified))))
 '(italic ((t (:background unspecified :foreground unspecified :underline nil))))
 '(region ((t (:background "#3b2e2a" :foreground "#c8c9db"))))
 '(secondary-selection ((t (:background "#e1e1e1"))))

 '(trailing-whitespace ((t (:background "#d9a0a0"))))

 '(match ((t (:background "#003b81" :foreground "#f0f0f0"))))
 '(isearch ((t (:weight normal :foreground "#aa3030" :background "palevioletred2"))))
 '(isearch-fail ((t (:background "#aa3030" :foreground "#c1c1c1" :weight bold))))
 '(lazy-highlight ((t (:inherit (match)))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))

 '(anzu-match-1 ((t (:inherit (match)))))
 '(anzu-match-2 ((t (:inherit (match)))))
 '(anzu-match-3 ((t (:inherit (match)))))
 '(anzu-mode-line ((t (:background unspecified :foreground "#101010" :weight bold))))

 ;; CODING
 '(font-lock-builtin-face ((t (:foreground "#512caf"))))
 '(font-lock-keyword-face ((t (:foreground "#512caf"))))
 '(font-lock-preprocessor-face ((t (:foreground "#512caf"))))

 '(font-lock-comment-face ((t (:foreground "#407326"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#407326"))))
 '(font-lock-warning-face ((t (:foreground "#407326" :weight bold))))

 '(font-lock-function-name-face ((t (:inherit (default)))))
 '(font-lock-variable-name-face ((t (:inherit (default)))))

 '(font-lock-doc-face ((t (:foreground "#3f3f00"))))
 '(font-lock-string-face ((t (:foreground "#3f3f00"))))

 '(font-lock-constant-face ((t (:foreground "#003b81" :weight bold))))

 '(font-lock-type-face ((t (:foreground "#b6111e"))))

 '(show-paren-match-face ((t (:background "#8cac8c" :inherit (default)))))
 '(show-paren-mismatch-face ((t (:background "#d9a0a0" :inherit (default)))))

 ;; ORG
 '(org-level-1 ((t (:foreground "#003b81" :height 300 :weight bold))))
 '(org-level-2 ((t (:foreground "#512caf" :height 200 :weight bold))))
 '(org-level-3 ((t (:foreground "#b6111e" :height 150 :weight bold))))
 '(org-level-4 ((t (:foreground "#3b2e2a" :weight bold))))
 '(org-hide ((t (:inherit (default) :inverse-video t))))
 '(org-warning ((t (:inherit (error)))))

 ;; UI
 '(company-scrollbar-bg ((t (:background "#2a1d19"))))
 '(company-scrollbar-fg ((t (:background "#e69f67"))))
 '(company-tooltip ((t (:height 120 :inherit (mode-line)))))
 '(company-tooltip-annotation ((t (:foreground "#e69f67"))))
 '(company-tooltip-common ((t (:foreground "#e69f67"))))
 '(company-tooltip-selection ((t (:background "#2a1d19" :foreground "#e69f67" :weight bold))))

 '(minibuffer-prompt ((t (:foreground "#003b81" :weight bold))))

 '(mode-line ((t (:background "#3b2e2a" :box nil :foreground "#c8c9db" :height 120))))
 '(mode-line-buffer-id ((t (:background unspecified :distant-foreground "#c8c9db" :foreground "#e69f67" :weight bold))))
 '(mode-line-inactive ((t (:background "#878777" :inherit (mode-line))))))
