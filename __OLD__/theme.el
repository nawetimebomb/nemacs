(add-to-list 'custom-theme-load-path (concat user-emacs-directory "__OLD__/themes/"))

(set-fontset-font t 'unicode (font-spec :name "Envy Code R-14") nil)
(set-face-font 'default "Envy Code R-14")

(load-theme 'monochrome-dark t)
