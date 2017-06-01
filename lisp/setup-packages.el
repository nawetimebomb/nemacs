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
(defconst installed-packages-list
  '(
    auto-complete ; auto-complete!
    better-defaults
    company ; another auto-complete
    company-quickhelp
    company-web ; complete my web stuff
    csharp-mode ; for game programming
    disable-mouse ; no mouse allowed!
    dumb-jump
    egg ; git support
    emmet-mode ; auto-complete my HTML tags!
    helm multi-line
    helm-projectile
    monokai-theme ; best theme right now!
    multiple-cursors
    nav-flash
    neotree ; file tree, looks good
    org ; org-mode is awesome
    powerline ; powerline rocks!
    project-explorer
    projectile ; project management at its best
    rainbow-mode ; color background for hex
    undo-tree
    web-beautify
    web-mode ; using with Javascript, JSX
    ztree
    )
  )

;; install selected package
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package installed-packages-list)
  (unless (package-installed-p package)
    (package-install package)))

;; done
(message "Packages sync finished")

(provide 'setup-packages)
