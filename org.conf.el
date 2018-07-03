(defun nemacs-get-inbox-file ()
  "Open my Inbox file."
  (interactive)
  (find-file nemacs-org-inbox-file))
(global-set-key (kbd "C-c i") #'nemacs-get-inbox-file)

(defun nemacs-get-org-file (filename)
  "Get the Org notes file that's shared between different devices. Concat the `filename' with the directory"
  (expand-file-name filename nemacs-notes-dir))

(defun nemacs-org-mark-done-and-archive ()
  "Mark a task as `DONE' and then archive it.
If it was already `DONE', keeps that state and doesn't change the `CLOSED' timestamp."
  (interactive)
  (let ((task-status (nth 2 (org-heading-components))))
    (when (or (not (eq task-status "DONE"))
              (not (eq task-status "CANCELED")))
      (org-todo 'done))
    (org-archive-subtree)))

(setq nemacs-org-archive-file (nemacs-get-org-file "archive.org")
      nemacs-org-inbox-file (nemacs-get-org-file "inbox.org")
      nemacs-org-meetings-file (nemacs-get-org-file "meetings.org")
      nemacs-org-project-file (nemacs-get-org-file "project.org")
      nemacs-org-someday-file (nemacs-get-org-file "someday.org"))

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
      org-id-files '((expand-file-name "org-id-locations" "~/Notes/references"))
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-log-done 'time
      org-tags-column -80)

(setq org-email-link-description-format "Email %c (%d): %s")

;; Capture
(setq org-todo-keywords
      '((sequence "TODO(t!)"
                  "STARTED(y!)"
                  "WAITING(w@)"
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
      '(("e" "Add entry below Emacs category"
         entry (file+headline nemacs-org-inbox-file "Emacs")
         "* TODO %?" :kill-buffer t)
        ("l" "Add entry below Linux category"
         entry (file+headline nemacs-org-inbox-file "Linux")
         "* TODO %?" :kill-buffer t)
        ("p" "Add entry below Personal category"
         entry (file+headline nemacs-org-inbox-file "Personal")
         "* TODO %?" :kill-buffer t)
        ("t" "Add entry below General Tasks category"
         entry (file+headline nemacs-org-inbox-file "Tasks")
         "* TODO %?" :kill-buffer t)
        ("w" "Add entry below Work category"
         entry (file+headline nemacs-org-inbox-file "Work")
         "* TODO %?" :kill-buffer t)
        ("m" "Start a clock for a meeting and log notes."
         entry (file nemacs-org-meetings-file)
         "* MEETING %U %^{Title} %^{attendees}p %^{location}p \n%?" :clock-in t :jump-to-captured t)))

;; Refile
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((nemacs-org-inbox-file :maxlevel . 2)
                           (nemacs-org-project-file :maxlevel . 2)
                           (nemacs-org-someday-file :maxlevel . 1)))

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
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)
(define-key org-mode-map [remap org-archive-subtree-default] #'nemacs-org-mark-done-and-archive)
