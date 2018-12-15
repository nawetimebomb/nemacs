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
                   org-plus-contrib
                   org-gcal
                   projectile
                   scss-mode
                   wanderlust))

  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))
