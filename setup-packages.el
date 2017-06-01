;;|==============================================|
;;|  title: setup-packages.el                    |
;;|  description: package repos/install config   |
;;|  copyright: elnawe.com (c) 2017              |
;;|==============================================|

;; initialize packages
(package-initialize)
(require 'package)

;; add package repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; define selected packages
(setq installed-package-list '(
    powerline egg disable-mouse emmet-mode company-web helm-projectile helm multi-line dumb-jump csharp-mode web-beautify web-mode company-quickhelp company auto-complete project-explorer projectile nav-flash multiple-cursors neotree org rainbow-mode undo-tree ztree better-defaults))

;; install selected package
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package installed-package-list)
  (unless (package-installed-p package)
    (package-install package)))
