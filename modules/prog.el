;;
;;; PACKAGES

(use-package company
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("M-<" . company-select-first)
        ("M->" . company-select-last))
  :custom
  (company-idle-delay 0.5))

(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package lsp-mode)

;;
;;; FUNCTION DEFINITIONS

(defun nemacs-setup-default-prog-mode ()
  "Setup the default programming mode for NEMACS."
  (interactive)
  (company-mode)
  (flycheck-mode))
