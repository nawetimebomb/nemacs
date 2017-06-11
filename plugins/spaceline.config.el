;; spaceline configuration file

(use-package spaceline
  :ensure t
  :config
  (spaceline-define-segment
   elnawe/version-control
   "Version control information."
   (propertize (when vc-mode
                 (defvar current-branch (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name))))))
                 (powerline-raw
                  (s-trim (concat current-branch
                                  (when (buffer-file-name)
                                    (pcase (vc-state (buffer-file-name))
                                      (`up-to-date "")
                                      (`edited " *")))))))
               'mouse-face nil))

  (spaceline-define-segment elnawe/which-function
    "Show the current function if exists"
    (when (and active
             (bound-and-true-p which-function-mode)
             (bound-and-true-p which-func-mode))
    (let* ((current (format-mode-line which-func-current)))
      (when (string-match "{\\(.*\\)}" current)
        (setq current (match-string 1 current)))
      (setq current (replace-regexp-in-string "%" "%%" current))
      (propertize current
                  'local-map which-func-keymap
                  'face 'which-func
                  'mouse-face nil))))

  (spaceline-define-segment elnawe/buffer-id
    "Name of buffer."
    (propertize (s-trim (powerline-buffer-id 'mode-line-buffer-id))
                'mouse-face nil))

  (spaceline-define-segment elnawe/major-mode
    "Show the current major mode"
    (propertize (powerline-major-mode)
                'mouse-face nil)))

(use-package spaceline-config
  :ensure nil
  :after spaceline
  :config
  (setq-default
   mode-line-format '("%e" (:eval (spaceline-ml-main)))
   spaceline-separator-dir-left '(left . left)
   spaceline-separator-dir-right '(right . right)
   spaceline-highlight-face-func 'spaceline-highlight-face-modified
   powerline-default-separator 'nil)
  (spaceline-emacs-theme)
  (spaceline-helm-mode 1)

  (spaceline-install
    '((elnawe/major-mode :face highlight-face)
      (projectile-root)
      (elnawe/buffer-id)
      (elnawe/which-function :when active)
      (anzu))
    '((selection-info)
      (elnawe/version-control)
      (line-column)
      (buffer-position)
      (global :face highlight-face)))

  (spaceline-install
    'helm
    '((helm-buffer-id :face spaceline-read-only)
      (helm-number)
      (helm-prefix-argument))
    '((global :face region)
      (helm-help)))
  (zenburn-with-color-variables
    (set-face-attribute 'powerline-active1 nil
                        :background zenburn-bg-1)

    (set-face-attribute 'powerline-active2 nil
                        :background zenburn-bg-1)

    (set-face-attribute 'mode-line-inactive nil
                        :background zenburn-bg)

    (set-face-attribute 'powerline-inactive1 nil
                        :background zenburn-bg)

    (set-face-attribute 'powerline-inactive2 nil
                        :background zenburn-bg)

    (set-face-attribute 'spaceline-modified nil
                        :background zenburn-red
                        :foreground zenburn-red-4)

    (set-face-attribute 'spaceline-unmodified nil
                        :background zenburn-green-1
                        :foreground zenburn-green+4)

    (set-face-attribute 'spaceline-read-only nil
                        :background zenburn-blue+1
                        :foreground zenburn-blue-5)))

(provide 'spaceline.config)
