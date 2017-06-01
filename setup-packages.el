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
(defconst installed-package-list
  '(
    auto-complete
    better-defaults
    company
    company-quickhelp
    company-web
    csharp-mode
    disable-mouse
    dumb-jump
    egg
    emmet-mode
    helm multi-line
    helm-projectile
    multiple-cursors
    nav-flash
    neotree
    org
    powerline
    project-explorer
    projectile
    rainbow-mode
    undo-tree
    web-beautify
    web-mode
    ztree
    )
  )

;; install selected package
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package installed-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; add plugin configurations to require list
(add-to-list 'load-path (concat user-emacs-directory "plugins/"))
