(require 'battery)
(require 'desktop)

(defconst nemacs-version "22")
(defconst nemacs-local-dir (concat user-emacs-directory ".local/"))
(defconst nemacs-cache-dir (concat nemacs-local-dir "cache/"))
(defconst nemacs-tmp-dir (concat nemacs-local-dir "tmp/"))
(defconst nemacs-directories (list nemacs-local-dir nemacs-cache-dir nemacs-tmp-dir))

(dolist (dir nemacs-directories)
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;;###autoload
(defmacro IS-LINUX (&rest body)
  `(when (eq system-type 'gnu/linux)
     ,@body))

;;;###autoload
(defmacro IS-WINDOWS (&rest body)
  `(when (memq system-type '(cygwin windows-nt ms-dos))
     ,@body))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(IS-WINDOWS (setq selection-coding-system 'utf-8))

(setq warning-minimum-level :emergency)

(setq create-lockfiles nil
      make-backup-files nil)

(setq-default custom-file (make-temp-file (expand-file-name "emacs-custom.el" nemacs-tmp-dir)))
(setq-default transient-history-file (expand-file-name "transient" nemacs-cache-dir))
(setq-default server-socket-dir nemacs-cache-dir)
(setq abbrev-file-name (concat nemacs-local-dir "abbrev.el")
      auto-save-list-file-name (concat nemacs-cache-dir "autosave")
      bookmark-default-file (concat nemacs-cache-dir   "bookmarks")
      nsm-settings-file (expand-file-name "ns.data" nemacs-cache-dir)
      pcache-directory (concat nemacs-cache-dir "pcache")
      recentf-save-file (expand-file-name "recentf" nemacs-cache-dir)
      savehist-file (expand-file-name "history" nemacs-cache-dir)
      url-history-file (expand-file-name "url.el" nemacs-cache-dir))

(setq bookmark-save-flag 1)

(setq desktop-auto-save-timeout 30
      desktop-base-file-name "emacs.desktop"
      desktop-base-lock-name "emacs.lock"
      desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\'\\)"
      desktop-modes-not-to-save '(dired-mode org-mode rjsx-mode special-mode
                                             tags-table-mode vc-dir-mode)
      desktop-load-locked-desktop nil
      desktop-path (list nemacs-cache-dir)
      desktop-save nil
      desktop-dirname nemacs-cache-dir)
(desktop-save-mode -1)
(ido-mode -1)
(recentf-mode 1)

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(eval-after-load 'x-win
  (let ((session-dir (concat nemacs-cache-dir "sessions/")))
    `(progn
       (make-directory ,session-dir t)
       (defun emacs-session-filename (session-id)
         (expand-file-name session-id ,session-dir)))))

(defun nemacs-prompt-before-exiting-emacs ()
  "Prompts before closing the frame with `C-x C-c'. Standarizes `emacs' and
`emacsclient'."
  (interactive)
  (let* ((tmp-files (directory-files nemacs-tmp-dir t "[a-z|A-Z]")))
    (if (y-or-n-p ">>> Quit Nemacs? ")
        (progn
          (dolist (tmp-file tmp-files)
            (delete-file tmp-file))
          (save-buffers-kill-terminal))
      (message "Good. You should never do it."))))

;; Remap keybindings
(global-set-key (kbd "C-x C-c") #'nemacs-prompt-before-exiting-emacs)

;; Ask for `y-or-n' instead of `yes-or-no'.
(fset #'yes-or-no-p #'y-or-n-p)

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(setq max-lisp-eval-depth 50000
      max-specpdl-size 13000)

;; Startup configuration
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'org)

(setq straight-use-package-by-default t)

(setq inhibit-default-init t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(defvar nemacs-large-file-size 1
  "Size (in MB) above which the user will be prompted to open the
    file literally to avoid performance issues. Opening literally
    means that no major or minor modes are active and the buffer
    is read-only.")

(defvar nemacs-large-file-modes-list '(archive-mode
                                       tar-mode
                                       jka-compr
                                       git-commit-mode
                                       image-mode
                                       doc-view-mode
                                       doc-view-mode-maybe
                                       ebrowse-tree-mode
                                       pdf-view-mode)
  "Major modes that `nemacs/check-large-file' will ignore")

;; Quit recursively when pressing `C-g'.
(defvar nemacs-escape-hook nil
  "A hook that runs after pressing `C-g' and before
    `keyboard-quit'")
(defun nemacs-escape ()
  "Runs the `nemacs-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'nemacs-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; runs the default quit.
        (t (keyboard-quit))))
(global-set-key [remap keyboard-quit] ;; C-g
                #'nemacs-escape)

;; A much better beginning-of-line method.
(defun nemacs-move-beginning-of-line ()
  "Move point to first non-whitespace character, or a beginning
of line."
  (interactive "^")
  (let ((origin (point)))
    (beginning-of-line)
    (and (= origin (point))
         (back-to-indentation))))
(global-set-key [remap move-beginning-of-line] ;; C-a
                #'nemacs-move-beginning-of-line)
(global-set-key [remap org-beginning-of-line] ;; C-a
                #'nemacs-move-beginning-of-line)

;; Kill current buffer without the prompt
(defun nemacs-kill-current-buffer ()
  "Kill current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") #'nemacs-kill-current-buffer)

;; Create a window and switch.
(defun nemacs-create-window-bottom-and-switch ()
  "Creates a new window to the bottom and then switch to it."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key [remap split-window-below] ;; C-x 2
                #'nemacs-create-window-bottom-and-switch)

;; Same as above, but this time create the window to the right.
(defun nemacs-create-window-right-and-switch ()
  "Creates a new window to the right and then switch to it."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key [remap split-window-right] ;; C-x 3
                #'nemacs-create-window-right-and-switch)

;; Check the file size and use `fundamental-mode' if the file is too large.
;; This prevents freeze whenever opening huge text files or images.
(defun nemacs-check-large-file ()
  "Check if the buffer's file is large (see `nemacs-large-file-size'). If so,
ask for confirmation to open it literally (read-only, disable undo and in
fundamental-mode) for performance sake."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and (not (memq major-mode nemacs-large-file-modes-list))
               size (> size (* 1024 1024 nemacs-large-file-size))
               (y-or-n-p
                (format (concat "%s is a large file, open literally to "
                                "avoid performance issues?")
                        (file-relative-name filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
(add-hook 'find-file-hook #'nemacs-check-large-file)

;;
;;; SAVING FILES

;; Default saving configurations
(setq auto-save-default nil
      delete-by-moving-to-trash t)
;; After saving, remove all trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;
;;; MINOR MODES

;; Columns and lines are shown in the modeline.
(setq column-number-mode t
      line-number-mode t)
(column-number-mode 1)
;; Show matching parens.
(show-paren-mode t)
;; Revert file to newest version if saved from another system (I.e. changes from VCS).
(global-auto-revert-mode t)
;; Jump subwords.
(global-subword-mode t)
;; Delete selection when overriden by new code.
(delete-selection-mode t)
;; Save search and navigation history
(savehist-mode t)

(setq-default fill-column 120
              indent-tabs-mode nil
              mouse-yank-at-point t
              require-final-newline t
              tab-always-indent t
              tab-width 4
              tabify-regexp "^\t* [ \t]+"
              truncate-lines nil)

;; Add TODOs, NOTEs and IMPORTANT keywords
(defvar fixme-modes '(c++-mode
                      c-mode
                      emacs-lisp-mode
                      js2-mode
                      typescript-mode
                      rjsx-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)

;; NOTE: Temporary face. Change this face within the theme configuration.
(modify-face 'font-lock-fixme-face "IndianRed" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "DodgerBlue" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face nil nil nil t nil t nil nil)

(setq mode-line-default-help-echo nil)
;; Header Line
(setq-default header-line-format nil)

;;
;;; FRAMES/WINDOWS

;; General and frame configurations
(setq-default frame-inhibit-implied-resize t
	          frame-title-format "NEMACS"
	          fringe-indicator-alist (delq
				                      (assq 'continuation fringe-indicator-alist)
				                      fringe-indicator-alist)
	          help-window-select t
	          resize-mini-windows 'grow-only
	          ring-bell-function #'ignore
	          show-help-function nil
	          split-height-threshold nil
	          split-width-threshold 160
	          uniquify-buffer-name-style 'post-forward-angle-brackets
	          use-dialog-box nil
	          visible-bell nil
	          word-wrap t)

;; Fix horizontal and vertical scrolling, specially in larger files. Scrolling in Emacs
;; is sometimes slow in larger files as it tries to center the view in
(setq auto-window-vscroll nil
      hscroll-margin 2
      hscroll-step 1
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t)

;; Blink cursor, it makes life simpler. This might conflict with some minor modes.
(blink-cursor-mode 1)
;; Don't blink the paren matching at point.
(setq blink-matching-paren nil)
;; Don't stretch the cursor to fit wide characters.
(setq x-stretch-cursor nil)
;; Show cursor only in current selected window.
(setq cursor-in-non-selected-windows nil)

(use-package ace-window
  :bind
  ("M-o" . ace-window))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-color-icons nil))

(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode))

(use-package consult
  :bind
  (("M-g g" . consult-goto-line)
   ("M-y"   . consult-yank-from-kill-ring)
   ("C-x b" . consult-buffer)
   ("C-x B" . consult-buffer-other-window)))

(use-package magit)

(use-package marginalia
  :bind
  ((:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package project
  :custom
  (project-list-file (expand-file-name "projects" nemacs-cache-dir)))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-scroll-margin 0))

(use-package vterm
  :bind
  (("C-x T" . vterm)))

(load (concat user-emacs-directory "programming.el"))

(use-package immaterial-theme
  :config
  (load-theme 'immaterial-dark t)
  (custom-set-faces
   '(region ((t (:background "#000099"))))))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t))

(use-package org-contrib
  :config
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  :custom
  (org-hide-emphasis-markers t))

(use-package org-present
  :init
  (defun nemacs-org-present-prepare-slide (buffer-name heading)
    (org-overview)
    (org-show-entry)
    (org-show-children))

  (defun nemacs-org-present-start ()
    (setq-local face-remapping-alist '((default (:height 1.5) fixed-pitch)
                                       (org-level-1 (:height 1.5) org-level-2)
                                       (header-line (:height 4.0) fixed-pitch)
                                       (org-document-title (:height 4.0) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) default)
                                       (org-block-begin-line (:height 0.7) org-block)))
    (setq header-line-format " ")
    (org-present-hide-cursor)
    (org-display-inline-images)
    (visual-fill-column-mode 1)
    (visual-line-mode 1))

  (defun nemacs-org-present-end ()
    (setq-local face-remapping-alist '((default fixed-pitch default)))
    (setq header-line-format nil)
    (org-present-show-cursor)
    (org-remove-inline-images)
    (visual-fill-column-mode 0)
    (visual-line-mode 0))
  :hook
  (org-present-mode . nemacs-org-present-start)
  (org-present-mode-quit . nemacs-org-present-end)
  (org-present-after-navigate-functions . nemacs-org-present-prepare-slide))


;; Add hoook to after-init
(add-hook 'after-init-hook
          #'(lambda ()
              (if (find-font (font-spec :name "Envy Code R"))
                  (progn
                    (set-fontset-font t 'unicode (font-spec :name "Envy Code R-18") nil)
                    (set-face-font 'default "Envy Code R-18")))
              (if (file-exists-p (concat user-emacs-directory "custom.el"))
                  (load (concat user-emacs-directory "custom.el")))
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1
                    read-process-output-max (* 1024 1024))))
