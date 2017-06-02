;; |==============================================|
;; |  title: modes.el                             |
;; |  description: modes toggler configuration    |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; global modes
(global-auto-revert-mode t)
(global-company-mode t)
(global-disable-mouse-mode t)
(global-hl-line-mode t)
(global-linum-mode t)

;; buffer modes
(column-number-mode t)
(desktop-save-mode t)
(emmet-mode t)
(projectile-mode t)
(rainbow-mode t)

;; autoload extensions modes
(autoload 'csharp-mode "csharp-mode" t)
(autoload 'web-mode "web-mode" t)
(setq auto-mode-alist '(
                        ("\\.cs\\'" . csharp-mode)
                        ("\\.js\\'" . web-mode)
                        ("\\.jsx\\'" . web-mode)
                        ("\\.el\\'" . lisp-mode)
                        ("\\.org\\'" . org-mode)
                        ("\\.md\\'" . org-mode)
                        ("\\.mkd\\'" . org-mode)))

(provide 'modes)
