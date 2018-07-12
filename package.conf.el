(setq package-archives '())

(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(dolist (package '(anzu
                   bbdb
                   company
                   company-c-headers
                   flycheck
                   helm
                   helm-ag
                   helm-projectile
                   js2-mode
                   json-mode
                   magit
                   md4rd
                   mu4e-alert
                   org-plus-contrib
                   ox-hugo
                   projectile
                   scss-mode
                   w3m))

  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))
