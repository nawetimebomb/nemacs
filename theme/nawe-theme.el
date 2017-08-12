(deftheme nawe
  "A nice low-contrast theme inspired in Zenburn (which I love),
Jonathan Blow's Emacs theme and this guy's theme
https://github.com/pixlark/JonathanBlowEmacsTheme")

;; NOTE: This theme is currently (and always) in progress.

(custom-theme-set-faces
 'nawe
 '(default ((t (:inherit nil :stipple nil :background "#2b2b2b" :foreground "tan" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "outline" :family "Monospace"))))
 '(bold ((t (:inherit (default) :weight bold))))
 '(italic ((t (:underline nil))))
 '(cursor ((t (:background "#54ff9f" :foreground "#2b2b2b"))))

 '(error ((t (:foreground "#ff3030" :underline nil :weight bold))))

 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Monospace" :foundry "outline" :width normal :height 100 :weight normal :slant normal))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "turquoise3" :weight normal))))
 '(highlight ((t (:background "#404040"))))
 '(region ((t (:background "#202020" :foreground "#ffffef" :weight bold))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((t (:background "red"))))

 '(dired-warning ((t (:inherit (error)))))

 '(font-lock-builtin-face ((t (:foreground "grey94"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#eee926"))))
 '(font-lock-comment-face ((t (:foreground "#eee926"))))
 '(font-lock-constant-face ((t (:foreground "cyan2"))))
 '(font-lock-doc-face ((t (:foreground "#bcc1c5"))))
 '(font-lock-function-name-face ((t (:foreground "burlywood"))))
 '(font-lock-keyword-face ((t (:foreground "grey94"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "grey94"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#bcc1c5"))))
 '(font-lock-type-face ((t (:foreground "SeaGreen1"))))
 '(font-lock-variable-name-face ((t (:foreground "burlywood"))))
 '(font-lock-warning-face ((t (:background nil :foreground "#eee926" :underline nil :weight bold))))

 '(show-paren-match-face ((t (:background "red" :foreground "black"))))
 '(link ((t (:underline t :foreground "blue"))))
 '(link-visited ((t (:underline nil))))
 '(button ((t (:inherit (link)))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(tooltip ((t (:foreground "systeminfotext" :background "systeminfowindow" :inherit (variable-pitch)))))

 '(mode-line ((t (:foreground "#202020" :background "burlywood" :box (:line-width -1 :color nil :style released-button)))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "#202020" :background "#3e3e5e"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:foreground "#2f2f2f" :background "grey75"))))

 '(match ((t (:background "#548b54" :foreground "#f0f0f0"))))
 '(isearch ((t (:weight normal :foreground "brown4" :background "IndianRed"))))
 '(isearch-fail ((t (:background "brown" :foreground "grey94" :weight bold))))
 '(lazy-highlight ((t (:inherit (match)))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))

 '(anzu-match-1 ((t (:inherit (match)))))
 '(anzu-match-2 ((t (:inherit (match)))))
 '(anzu-match-3 ((t (:inherit (match)))))
 '(anzu-mode-line ((t (:inherit (mode-line) :weight bold))))

 '(org-level-1 ((t (:foreground "grey74"))))
 '(org-level-2 ((t (:foreground "#54ff9f"))))
 '(org-level-3 ((t (:foreground "turquoise3"))))
 '(org-level-4 ((t (:foreground "grey54"))))
 '(org-warning ((t (:inherit (error)))))

 '(js2-error ((t (:foreground "red"))))
 '(js2-function-param ((t (:inherit (default)))))
 '(js2-external-variable ((t (:inherit (default))))))

(provide-theme 'nawe)
