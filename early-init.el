(add-to-list 'default-frame-alist '(fullscreen . maximized))

(tooltip-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq package-enable-at-startup nil)
(setq straight-fix-flycheck t)

(if (find-font (font-spec :name "Envy Code R"))
    (progn
      (set-fontset-font t 'unicode (font-spec :name "Envy Code R-18") nil)
      (set-face-font 'default "Envy Code R-18")))
