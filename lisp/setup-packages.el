;;|==============================================|
;;|  title: setup-packages.el                    |
;;|  description: package repos/install config   |
;;|  copyright: elnawe.com (c) 2017              |
;;|==============================================|

;; initialize packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; define selected packages
(defconst installed-packages-list
  '(
    better-defaults ; remove menus
    company ; another auto-complete
    company-quickhelp
    company-web ; complete my web stuff
    csharp-mode ; for game programming
    disable-mouse ; no mouse allowed!
    dumb-jump
    egg ; git support
    emmet-mode ; auto-complete my HTML tags!
    find-file-in-project ; very large projects
    git-gutter ; mark the diff lines
    helm ; better buffer, open file
    helm-swoop ; search between buffers
    js2-mode ; new Javascript mode
    json-mode ; read json!
    monokai-theme ; best theme right now!
    multi-line
    multiple-cursors
    nav-flash
    neotree ; file tree, looks good
    org ; org-mode is awesome
    project-explorer
    projectile ; project management at its best
    rainbow-mode ; color background for hex
    rjsx-mode ; jsx support
    spaceline ; better than powerline
    undo-tree
    use-package ; best thing in life!
    web-beautify
    web-mode ; using with Javascript, JSX
    ztree
    )
  )

;; install packages from list
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package installed-packages-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'setup-packages)
