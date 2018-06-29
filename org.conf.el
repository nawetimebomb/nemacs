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

;; Defaults
(setq org-agenda-files (list nemacs-org-inbox-file)
      org-agenda-start-on-weekday 0
      org-archive-location (concat nemacs-org-archive-file "::* From %s")
      org-clock-in-resume t
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-persist t
      org-clock-persist-query-resume nil
      org-deadline-warning-days 7
      org-default-notes-file nemacs-org-inbox-file
      org-drawers '("PROPERTIES" "LOGBOOK")
      org-id-files '((expand-file-name "org-id-locations" "~/Notes/references"))
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-log-done 'time
      org-tags-column -80)

;; Capture
(setq org-todo-keywords '((sequence "TODO(t)" "CURRENT(y)" "WAITING(w@)" "|" "DONE(d)" "CANCELED(c@)"))
      org-capture-templates '(("t" "Add a Todo in the Inbox"
                               entry (file nemacs-org-inbox-file)
                               "* TODO %i%?" :kill-buffer t)
                              ("T" "Timebomb! Something that has a clear deadline or schedule"
                               entry (file nemacs-org-inbox-file)
                               "* TODO %i%? \nSCHEDULED: %^t" :kill-buffer t)
                              ("m" "Start a clock for a meeting and log notes."
                               entry (file nemacs-org-meetings-file)
                               "* MEETING %U %^{Title} %^{attendees}p %^{location}p \n%?" :clock-in t :jump-to-captured t)
                              ("p" "Current project task"
                               entry (file+headline nemacs-org-project-file "Tasks")
                               "* TODO %i%?" :kill-buffer t)
                              ("r" "ROR - Things to fix"
                               table-line (file+headline (nemacs-get-org-file "ror.org") "Team's Chart")
                               "| %u | %^{Level} | %^{Reporter} | %^{Person} | %^{Action|Missing Meeting} | %^{Comments} |")))
;; Refile
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((nemacs-org-inbox-file :maxlevel . 2)
                           (nemacs-org-project-file :maxlevel . 2)
                           (nemacs-org-someday-file :maxlevel . 1)))

;; Tags
(setq org-tag-persistent-alist '(("@emacs" . ?e)
                                 ("@errands" . ?e)
                                 ("@girlfriend" . ?g)
                                 ("@outdoors" . ?o)
                                 ("@weekend" . ?z)
                                 ("@work" . ?w)))

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

(require 'org-notmuch)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)
(define-key org-mode-map [remap org-archive-subtree-default] #'nemacs-org-mark-done-and-archive)
