;;; Nemacs

;;; Commentary:
;; The entry point into Nemacs.

;;; Code:

(advice-add #'display-startup-echo-area-message :override #'ignore)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      mode-line-format nil)

(let ((file-name-handler-alist nil))
  (if (file-exists-p (expand-file-name "nemacs.elc" user-emacs-directory))
      (load-file (expand-file-name "nemacs.elc" user-emacs-directory))
    (require 'org)
    (org-babel-load-file (expand-file-name "nemacs.org" user-emacs-directory))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#252525" "#DADADA" "#C3C3C3" "#F6F6F6" "#E8E8E8" "#DADADA" "#F1F1F1" "#F6F6F6"])
 '(custom-safe-themes
   (quote
    ("1487a5a2ab24fa5205797f3a6620a9640d22ca63726c57776a8ff38ebf1b6b36" "086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" "444238426b59b360fb74f46b521933f126778777c68c67841c31e0a68b0cc920" "0e0c37ee89f0213ce31205e9ae8bce1f93c9bcd81b1bcda0233061bb02c357a8" default)))
 '(fci-rule-color "#171717")
 '(package-selected-packages
   (quote
    (tide rjsx-mode json-mode js2-mode helm-projectile projectile anzu tao-theme helm spaceline all-the-icons neotree f async use-package)))
 '(projectile-mode t nil (projectile))
 '(show-paren-mode t)
 '(vc-annotate-background "#0E0E0E")
 '(vc-annotate-color-map
   (quote
    ((20 . "#616161")
     (40 . "#9E9E9E")
     (60 . "#9E9E9E")
     (80 . "#C3C3C3")
     (100 . "#C3C3C3")
     (120 . "#DADADA")
     (140 . "#DADADA")
     (160 . "#E8E8E8")
     (180 . "#E8E8E8")
     (200 . "#E8E8E8")
     (220 . "#F1F1F1")
     (240 . "#F1F1F1")
     (260 . "#F1F1F1")
     (280 . "#F6F6F6")
     (300 . "#F6F6F6")
     (320 . "#F6F6F6")
     (340 . "#FAFAFA")
     (360 . "#FAFAFA"))))
 '(vc-annotate-very-old-color "#DADADA"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
