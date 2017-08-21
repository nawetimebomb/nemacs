(deftheme nawe
  "A nice low-contrast theme inspired in Zenburn (which I love),
Jonathan Blow's Emacs theme and this guy's theme
https://github.com/pixlark/JonathanBlowEmacsTheme")

;; NOTE: This theme is currently (and always) in progress.

(custom-theme-set-faces
 'nawe
 '(default ((t (:inherit nil :stipple nil :background unspecified :foreground unspecified :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "outline" :family "Monospace"))))
 (when window-system
   '(default ((t (:inherit (default) :background "#322b32" :foreground "#a9a999" :height 95)))))
 '(bold ((t (:background unspecified :foreground unspecified :weight bold))))
 '(italic ((t (:background unspecified :foreground unspecified :underline nil))))
 '(cursor ((t (:background "#54ff9f" :foreground "#2b2b2b"))))
 '(error ((t (:foreground "#ff3030" :underline nil :weight bold))))

 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Monospace" :foundry "outline" :width normal :height 100 :weight normal :slant normal))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "turquoise3" :weight normal))))
 '(highlight ((t (:background "#404040"))))
 '(region ((t (:background "#00008b" :foreground unspecified))))
 '(shadow ((t (:foreground "grey57"))))
 '(secondary-selection ((t (:foreground "SkyBlue4"))))
 '(trailing-whitespace ((t (:background "#d9a0a0"))))

 '(dired-warning ((t (:inherit (error)))))

 '(font-lock-builtin-face ((t (:foreground "grey94"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#eee926"))))
 '(font-lock-comment-face ((t (:foreground "#eee926"))))
 '(font-lock-constant-face ((t (:foreground "#00c5cd"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:inherit (default)))))
 '(font-lock-keyword-face ((t (:foreground "grey94"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "grey94"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#d2b48c"))))
 '(font-lock-type-face ((t (:foreground "#66cdaa"))))
 '(font-lock-variable-name-face ((t (:inherit (default)))))
 '(font-lock-warning-face ((t (:background nil :foreground "#eee926" :underline nil :weight bold))))

 '(show-paren-match-face ((t (:background "#8cac8c" :foreground "#000000"))))
 '(show-paren-mismatch-face ((t (:background "#d9a0a0" :foreground "#000000"))))
 '(link ((t (:underline t :foreground "#6495ed"))))
 '(link-visited ((t (:underline nil))))
 '(button ((t (:inherit (link)))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(tooltip ((t (:foreground "systeminfotext" :background "systeminfowindow" :inherit (variable-pitch)))))

 '(mode-line ((t (:background "#0087af" :foreground "black" :box (:line-width -1 :color nil :style released-button)))))
 '(mode-line-buffer-id ((t (:background unspecified :foreground "#101010" :weight bold))))
 '(mode-line-inactive ((t (:inherit (mode-line) :background "#a9a999"))))

 '(match ((t (:background "paleturquoise4" :foreground "#f0f0f0"))))
 '(isearch ((t (:weight normal :foreground "brown4" :background "palevioletred2"))))
 '(isearch-fail ((t (:background "brown" :foreground "grey94" :weight bold))))
 '(lazy-highlight ((t (:inherit (match)))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))

 '(anzu-match-1 ((t (:inherit (match)))))
 '(anzu-match-2 ((t (:inherit (match)))))
 '(anzu-match-3 ((t (:inherit (match)))))
 '(anzu-mode-line ((t (:background unspecified :foreground "#101010" :weight bold))))

 '(org-level-1 ((t (:foreground "grey74"))))
 '(org-level-2 ((t (:foreground "#54ff9f"))))
 '(org-level-3 ((t (:foreground "turquoise3"))))
 '(org-level-4 ((t (:foreground "grey54"))))
 '(org-hide ((t (:inherit (default) :inverse-video t))))
 '(org-warning ((t (:inherit (error)))))

 '(js2-error ((t (:foreground "red"))))
 '(js2-function-param ((t (:inherit (default)))))
 '(js2-external-variable ((t (:inherit (default))))))

(provide-theme 'nawe)
