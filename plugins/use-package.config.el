;; A use-package declaration for simplifying your .emacs by @jwiegley.
;; https://github.com/jwiegley/use-package
;; configuration file by @elnawe.

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(unless (and (package-installed-p 'delight)
             (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'delight t)
  (package-install 'use-package t))
(setq-default
 use-package-always-defer t
 use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(provide 'use-package.config)
