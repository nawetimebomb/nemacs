(defun nemacs-helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as the input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(setq helm-split-window-inside-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(add-hook 'helm-minibuffer-set-up-hook #'nemacs-helm-hide-minibuffer-maybe)

(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key [remap find-file] #'helm-find-files)
(global-set-key [remap switch-to-buffer] #'helm-mini)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)