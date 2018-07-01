(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(dolist (package '(anzu
                   elfeed
                   helm
                   helm-notmuch
                   helm-projectile
                   js2-mode
                   notmuch
                   org-plus-contrib
                   projectile
                   w3m
                   ))

  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))
