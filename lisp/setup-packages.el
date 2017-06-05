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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; define selected packages
(defconst installed-packages-list
  '(
    anzu ; display search results
    better-defaults ; remove menus
    color-theme-sanityinc-tomorrow ; tomorrow theme
    column-marker ; mark column width
    company ; auto-complete
    company-quickhelp
    company-web ; complete my web stuff
    csharp-mode ; for game programming
    disable-mouse ; no mouse allowed!
    dumb-jump ; go to definition
    emmet-mode ; auto-complete my HTML tags!
    find-file-in-project ; very large projects
    git-gutter ; mark the diff lines
    helm ; better buffer, open file
    helm-swoop ; search between buffers
    js2-mode ; new Javascript mode
    json-mode ; read json!
    magit ; git support
    monokai-theme ; best theme right now!
    multi-line
    multiple-cursors
    nav-flash
    neotree ; file tree, looks good
    org ; org-mode is awesome
    powerline ; needed for spaceline
    project-explorer
    projectile ; project management at its best
    rainbow-mode ; color background for hex
    rjsx-mode ; jsx support
    smartparens ; parenthesis!
    spaceline ; better than powerline
    undo-tree
    use-package ; best thing in life!
    web-beautify
    web-mode ; using with Javascript, JSX
    ztree
    )
  )

(provide 'setup-packages)
