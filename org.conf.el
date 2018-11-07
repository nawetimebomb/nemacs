(defun nemacs-get-inbox-file ()
  "Open my Inbox file. If I'm in office hours, focus only on the Work category and show me that. If not, show me everything."
  (interactive)
  (let ((time-hour (string-to-number (format-time-string "%H")))
        (day-of-week (format-time-string "%a"))
        (day-of-weekend '("Sat" "Sun"))
        (min-hour 7)
        (max-hour 18))
    (kill-buffer "inbox.org")
    (find-file nemacs-org-inbox-file)
    (when (and (> time-hour min-hour)
               (< time-hour max-hour)
               (not (member day-of-week day-of-weekend)))
        (progn (org-global-cycle 1)
               (goto-char (org-find-entry-with-id "2e99b399-8521-49eb-9f9b-d9cdaf3077aa"))
               (org-cycle)))))
(global-set-key (kbd "C-c i") #'nemacs-get-inbox-file)

(defun nemacs-get-org-file (filename)
  "Get the Org notes file that's shared between different devices. Concat the `filename' with the directory"
  (expand-file-name filename nemacs-notes-dir))

(setq nemacs-org-archive-file (nemacs-get-org-file "archive.org")
      nemacs-org-inbox-file (nemacs-get-org-file "inbox.org")
      nemacs-org-meetings-file (nemacs-get-org-file "meetings.org")
      nemacs-org-project-file (nemacs-get-org-file "project.org")
      nemacs-org-someday-file (nemacs-get-org-file "someday.org"))

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
      '(("b" "Add entry below Books category"
         entry (file+headline nemacs-org-inbox-file "Books")
         "* %?" :kill-buffer t)
        ("e" "Add entry below Emacs category"
         entry (file+headline nemacs-org-inbox-file "Emacs")
         "* TODO %?" :kill-buffer t)
        ("g" "Add entry below Games category"
         entry (file+headline nemacs-org-inbox-file "Games")
         "* %?" :kill-buffer t)
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
        ("y" "PROJECT: Add entry to project.org"
         entry (file+headline nemacs-org-project-file "Tasks")
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
