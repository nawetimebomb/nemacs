(add-hook
 'prog-mode-hook #'(lambda ()
                    (company-mode)
                    (display-line-numbers-mode)))
