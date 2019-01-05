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

(setq nemacs-basic-packages '(anzu
                              js2-mode
                              json-mode
                              projectile
                              scss-mode))

(setq nemacs-extra-packages '(boxquote
                              company
                              company-c-headers
                              dashboard
                              flycheck
                              helm
                              helm-ag
                              helm-projectile
                              ledger-mode
                              magit
                              org-bullets
                              org-gcal
                              org-journal
                              org-plus-contrib
                              sane-term
                              w3m))

(dolist (package (append nemacs-basic-packages
                         (when nemacs-enable-extras
                           nemacs-extra-packages)))

  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))
