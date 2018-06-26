;; set 2 spaces indentation.
;; `json-mode' uses the same variable that `js-mode' and `js2-mode' are using to indent.
;; so we hook this setting to the `json-mode' so JavaScript files are not affected.
(add-hook 'json-mode-hook (lambda ()
                            (make-local-variable 'js-indent-level)
                            (setq js-indent-level 2)))
