(defun nemacs-get-inbox-file ()
  "Open my Inbox file."
  (interactive)
  (find-file nemacs-org-inbox-file))

(defun nemacs-get-org-file (filename)
  "Get the Org notes file that's shared between different devices. Concat the `filename' with the directory"
  (expand-file-name filename nemacs-notes-dir))

(defun nemacs-capture-todo ()
  (interactive)
  "Creates new TODO capture"
  (org-capture nil "t"))

(setq nemacs-org-archive-file (nemacs-get-org-file "archive.org")
      nemacs-org-inbox-file (nemacs-get-org-file "inbox.org"))

(setq org-id-files '((expand-file-name "org-id-locations" "~/Notes/references"))
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(require 'org-id)

(setq org-enforce-todo-dependencies t)

(setq org-archive-location (concat nemacs-org-archive-file "::* From %s")
      org-clock-in-resume t
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-persist t
      org-clock-persist-query-resume nil
      org-deadline-warning-days 7
      org-default-notes-file nemacs-org-inbox-file
      org-directory nemacs-notes-dir
      org-log-done 'time
      org-tags-column -80)

(setq org-email-link-description-format "Email %c: %s")

;; Capture
(setq org-todo-keywords
      '((sequence "TODO(t!)"
                  "STARTED(s!)"
                  "WAITING(w@)"
                  "DELEGATED(m@)"
                  "FEEDBACK(f!/@)"
                  "REWORK(r@/!)"
                  "|"
                  "DONE(d)"
                  "CANCELED(c@)")
        (sequence "PROJECT(j!)" "|" "CANCELED(c@)" "DONE(d!)")))

(add-hook 'org-capture-before-finalize-hook
          (lambda ()
            (interactive)
            (org-set-property "CREATED" (format-time-string "<%Y-%m-%d %a %H:%M>"))))

(setq org-capture-templates
      '(("t" "Add a new TODO entry"
         entry (file nemacs-org-inbox-file)
         "* TODO %?" :kill-buffer t)))

;; Refile
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((nemacs-org-inbox-file :maxlevel . 2)))

;; Tags
(setq org-tag-persistent-alist '(("Emacs" . ?e)
                                 ("Org" . ?o)
                                 ("Work" . ?w)
                                 ("Books" . ?b)))

(setq org-descriptive-links t
      org-ellipsis "\u21b4"
      org-fontify-done-headline t
      org-fontify-whole-heading-line t
      org-image-actual-width nil
      org-startup-folded nil
      org-startup-truncated nil
      org-support-shift-select 'always)

(setq org-edit-src-content-indentation 0
      org-edit-src-persistent-message nil
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window)

(add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook #'hl-line-mode)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'nemacs-capture-todo)
(global-set-key (kbd "C-c i") #'nemacs-get-inbox-file)
(global-set-key (kbd "C-c l") #'org-store-link)
