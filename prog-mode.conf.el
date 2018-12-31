(defcustom nemacs-programming-modes '(c-mode
                                      c++-mode
                                      js2-mode
                                      json-mode
                                      prog-mode
                                      rjsx-mode
                                      scss-mode
                                      sgml-mode)
  "Major modes for programming.")

(add-to-list 'auto-mode-alist `(,(rx ".js" string-end)     . js-mode))
(add-to-list 'auto-mode-alist `(,(rx ".json" string-end)   . json-mode))
(add-to-list 'auto-mode-alist `(,(rx ".jsx" string-end)    . rjsx-mode))
(add-to-list 'auto-mode-alist `(,(rx ".less" string-end)   . scss-mode))
(add-to-list 'auto-mode-alist `(,(rx ".sass" string-end)   . scss-mode))
(add-to-list 'auto-mode-alist `(,(rx ".scss" string-end)   . scss-mode))

(defun nemacs-setup-programming-mode ()
  "Setup my defaults when programming."
  (setq show-trailing-whitespace t)
  (flyspell-prog-mode)
  (flycheck-mode 1))

(defun nemacs-c-mode-setup ()
  "Setup mode: `c-mode'."
  (nemacs-setup-programming-mode)
  (company-mode)
  (add-to-list 'company-backends 'company-c-headers)
  (c-set-style "linux")
  (set (make-local-variable 'c-basic-offset) 4)
  (define-key c-mode-map (kbd "<f8>")
    (defun nemacs-compile-build ()
      (interactive)
      (compile "make build")))
  (define-key c-mode-map (kbd "<S-f8>")
    (defun nemacs-compile-run ()
      (interactive)
      (compile "make run"))))

(defun nemacs-c++-mode-setup ()
  (nemacs-c-mode-setup)
  (define-key c++-mode-map (kbd "<f8>")
    (defun nemacs-compile-build ()
      (interactive)
      (compile "make -C ../")))
  (define-key c++-mode-map (kbd "<S-f8>")
    (defun nemacs-compile-run ()
      (interactive)
      (compile "make -C ../ run"))))

(defun nemacs-js2-mode-setup ()
  "Setup mode: `js2-mode'."
  (nemacs-setup-programming-mode)
  (setq js-indent-level 4
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

(defun nemacs-json-mode-setup ()
  "Setup mode: `json-mode'."
  (nemacs-setup-programming-mode)
  (set (make-local-variable 'js-indent-level) 2))

(defun nemacs-prog-mode-setup ()
  "Setup mode: `prog-mode'."
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|NOTE\\|TODO\\|BUG\\)"
                             1 'font-lock-warning-face prepend))))

(defun nemacs-rjsx-mode-setup ()
  "Setup mode: `rjsx-mode'."
  (setq js-indent-level 4
        sgml-basic-offset 4))

(defun nemacs-scss-mode-setup ()
  "Setup mode: `scss-mode'."
  (nemacs-setup-programming-mode)
  (setq css-indent-offset 4))

(defun nemacs-sgml-mode-setup ()
  "Setup mode: `sgml-mode'."
  (nemacs-setup-programming-mode)
  (set (make-local-variable 'sgml-basic-offset) 4))

(dolist (mode nemacs-programming-modes)
  (let ((hook-string (concat (symbol-name mode) "-hook"))
        (function-string (concat "nemacs-" (symbol-name mode) "-setup")))
    (add-hook (intern hook-string) (intern function-string))))
