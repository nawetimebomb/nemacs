(deftheme monochrome-dark
  "A black background, gray-scale and white theme.")

(let
    ((background           "#131313")
     (standard-text        "#f7f7f7")
     (very-high-contrast   "#cfcfcf")
     (high-contrast        "#9e9e9e")
     (medium-contrast      "#808080")
     (low-contrast         "#4a4a4a")
     (very-low-contrast    "#242424")
     (windows-blue         "#0b5ee3")
     (bloody-red           "#ff7f7f")
     (zenburn-green        "#779a77"))

  (custom-theme-set-faces
   'monochrome-dark
   ;; Base
   `(default ((t (:background ,background :box nil :font "DejaVu Sans Mono-10" :foreground ,standard-text))))
   `(bold ((t (:background unspecified :foreground unspecified :weight bold))))
   `(cursor ((t (:background ,very-high-contrast :distant-foreground ,very-low-contrast))))
   `(italic ((t (:background unspecified :foreground unspecified :underline nil))))
   `(region ((t (:background ,windows-blue :foreground ,standard-text))))
   `(secondary-selection ((t (:background ,low-contrast))))
   `(highlight ((t (:background ,very-low-contrast))))
   `(whitespace-trailing ((t (:background ,low-contrast))))

   ;; Search
   `(match ((t (:background unspecified :foreground unspecified :underline t :weight bold))))
   `(isearch ((t (:background ,standard-text :foreground ,background))))
   `(isearch-fail ((t (:background unspecified :foreground ,medium-contrast :slant italic :weight bold))))
   `(lazy-highlight ((t (:inherit (match)))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))
   `(anzu-match-1 ((t (:inherit (match)))))
   `(anzu-match-2 ((t (:inherit (match)))))
   `(anzu-match-3 ((t (:inherit (match)))))
   `(anzu-mode-line ((t (:background unspecified :weight bold))))

   ;; Coding
   `(font-lock-builtin-face ((t (:foreground ,medium-contrast :weight normal))))
   `(font-lock-keyword-face ((t (:foreground ,medium-contrast :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,medium-contrast :weight normal))))
   `(font-lock-constant-face ((t (:foreground ,medium-contrast :weight normal))))

   `(font-lock-comment-face ((t (:foreground ,medium-contrast :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,medium-contrast :slant italic))))
   `(font-lock-warning-face ((t (:foreground ,medium-contrast :slant italic :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,medium-contrast :slant italic))))

   `(font-lock-function-name-face ((t (:inherit (default)))))
   `(font-lock-variable-name-face ((t (:inherit (default)))))

   `(font-lock-type-face ((t (:foreground ,medium-contrast))))
   `(font-lock-string-face ((t (:foreground ,medium-contrast))))

   `(js2-external-variable ((t (:foreground unspecified :inherit (default)))))
   `(js2-function-param ((t (:foreground unspecified :inherit (default)))))
   `(js2-jsdoc-tag ((t (:foreground ,medium-contrast :slant italic :weight bold))))
   `(typescript-jsdoc-tag ((t (:foreground ,medium-contrast :slant italic :weight bold))))
   `(js2-jsdoc-value ((t (:foreground ,medium-contrast :slant italic :weight normal))))
   `(typescript-jsdoc-tag ((t (:foreground ,medium-contrast :slant italic :weight bold))))
   `(js2-error ((t (:foreground ,bloody-red))))

   `(show-paren-match-face ((t (:background ,very-low-contrast :inherit (default)))))
   `(show-paren-mismatch-face ((t (:background ,bloody-red :foreground ,very-low-contrast :inherit (default)))))
   `(tide--hl-highlight ((t (:background ,zenburn-green))))

   ;; Org
   `(org-date ((t (:foreground ,medium-contrast))))
   `(org-ellipsis ((t (:foreground ,windows-blue))))
   `(org-headline-done ((t (:foreground unspecified :strike-through t))))
   `(org-hide ((t (:inherit (default) :inverse-video t))))
   `(org-level-1 ((t (:background unspecified :foreground ,high-contrast :weight bold))))
   `(org-level-2 ((t (:foreground ,medium-contrast :weight bold))))
   `(org-level-3 ((t (:foreground ,low-contrast :weight bold))))
   `(org-level-4 ((t (:foreground ,low-contrast :weight normal))))
   `(org-link ((t (:background unspecified :foreground ,windows-blue))))
   `(org-property-value ((t (:foreground ,medium-contrast :weight normal))))
   `(org-special-keyword ((t (:foreground ,low-contrast :weight bold))))
   `(org-tag ((t (:background ,very-low-contrast :foreground ,high-contrast))))
   `(org-warning ((t (:foreground ,bloody-red))))

   ;; UI
   `(company-scrollbar-bg ((t (:background ,medium-contrast))))
   `(company-scrollbar-fg ((t (:background ,low-contrast))))
   `(company-tooltip ((t (:background ,very-low-contrast :foreground ,standard-text))))
   `(company-tooltip-annotation ((t (:foreground ,medium-contrast))))
   `(company-tooltip-common ((t (:foreground ,high-contrast))))
   `(company-tooltip-selection ((t (:background ,low-contrast :foreground ,standard-text :weight bold))))
   `(company-tooltip-mouse ((t (:inherit (company-tooltip-selection)))))
   `(minibuffer-prompt ((t (:foreground ,standard-text :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,windows-blue))))
   `(neo-header-face ((t (:inherit (default)))))
   `(neo-root-dir-face ((t (:inherit (default) :weight bold))))
   `(neo-vc-edited-face ((t (:foreground ,zenburn-green))))
   `(neo-vc-up-to-date-face ((t (:inherit (default)))))

   ;; Mode-line
   `(mode-line ((t (:background ,zenburn-green :box nil :foreground ,standard-text))))
   `(powerline-active1 ((t (:background ,very-high-contrast :foreground ,very-low-contrast))))
   `(powerline-active2 ((t (:inherit (powerline-active1)))))
   `(spaceline-highlight-face ((t (:inherit (mode-line)))))
   `(spaceline-modified ((t (:background ,bloody-red :foreground ,background :inherit (mode-line) :weight bold))))
   `(spaceline-unmodified ((t (:foreground ,background :inherit (mode-line) :weight bold))))
   `(spaceline-read-only ((t (:background ,windows-blue :foreground ,background :inherit (mode-line) :weight bold))))
   `(mode-line-inactive ((t (:inherit (powerline-active1)))))
   `(powerline-inactive1 ((t (:inherit (powerline-active1)))))
   `(powerline-inactive2 ((t (:inherit (powerline-active1)))))))
