;; `json-mode' uses the same variable that `js-mode' and `js2-mode' are using to indent.
;; so we hook this setting to the `json-mode' so JavaScript files are not affected.
(add-hook 'json-mode-hook (lambda ()
                            (make-local-variable 'js-indent-level)
                            (setq js-indent-level 2)))

(add-hook 'js2-mode-hook (lambda ()
                           (setq js-indent-level 4
                                 js2-mode-show-parse-errors nil
                                 js2-mode-show-strict-warnings nil)))
