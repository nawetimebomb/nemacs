;; Modular in-buffer completion framework for Emacs by @company-mode
;; https://github.com/company-mode/company-mode
;; configuration file by @elnawe.

(use-package company
  :ensure t
  :config
  (global-company-mode)
  :bind
  ("M-n" . company-complete))

(provide 'company.config)
