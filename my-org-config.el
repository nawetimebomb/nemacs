;;
;;  my-org-config.el
;;  Author: Nahuel Jesus Sacchetti
;;

(defun elnawe::get-org-file (filename)
  "Gets Org notes file that's shared between different devices"
  (if (eq system-type 'windows-nt)
      (expand-file-name (concat "c:/Users/" user-login-name "/Notes/" filename ".org"))
    (concat "~/Notes/" filename ".org")))

(defun elnawe::prompt-to-clock-in ()
  "Prompt if you want to clock-in with the task tag or name"
  (interactive)
  (let ((task-info (or
                    (nth 5 (org-heading-components))   ;; Shows the tag or JIRA issue id.
                    (nth 4 (org-heading-components))   ;; Shows the task name.
                    )))
     (if (y-or-n-p (concat "Start time for task: " task-info))
         (org-clock-in))))

(use-package org
  :ensure nil
  :bind
  (:map org-mode-map
        ("C-c d" . org-todo)
        ("C-c st" . elnawe::prompt-to-clock-in))
  :config
  ;; Variables
  (defvar org-journal-file (elnawe::get-org-file "journal"))
  (defvar org-meetings-file (elnawe::get-org-file "meetings"))
  (defvar org-tasks-file (elnawe::get-org-file "tasks"))

  ;; Fontify
  (setq
   org-fontify-done-headline t
   org-fontify-whole-heading-line t)

  ;; Agenda
  (setq
   org-agenda-files (list org-tasks-file org-meetings-file)
   org-deadline-warning-days 7
   org-default-notes-file org-tasks-file
   org-descriptive-links nil
   org-ellipsis "\u21b4"
   org-image-actual-width nil
   org-log-done 'time
   org-startup-folded nil
   org-startup-truncated nil
   org-support-shift-select 'always)

  ;; Tasks
  (setq org-todo-keywords
        '((sequence "TODO" "INPROGRESS" "|" "DONE")
          (list "URGENT" "CANCELED" "MEETING"))
        org-todo-keyword-faces '(("TODO" :foreground "#cc9393" :weight bold)
                                 ("INPROGRESS" :foreground "#f0dfaf" :weight bold)
                                 ("DONE" :foreground "#afd8af" :weight bold)
                                 ("URGENT" :foreground "#dc8cc3" :weight bold)
                                 ("CANCELED" :foreground "#ac7373" :weight bold)
                                 ("MEETING" :foreground "#8cd0d3" :weight bold))
        org-capture-templates '(("t" "Add a Todo"
                                 entry (file+headline org-tasks-file "Tasks")
                                 "** TODO %? \nSCHEDULED: %T" :kill-buffer t)
                                ("u" "Urgent task!"
                                 entry (file+headline org-tasks-file "Urgent")
                                 "** URGENT [#A] %?\nDEADLINE: %\1" :time-prompt t :kill-buffer t)
                                ("j" "Add entry to the journal"
                                 item (file+datetree org-journal-file)
                                 "%u %?" :kill-buffer t)
                                ("m" "Add meeting notes"
                                 entry (file org-meetings-file)
                                 "* MEETING %^{Title} %^g \n Meeting with: %^{Meeting with}\n%?" :kill-buffer t)
                                ("c" "Close clocked task and add description. Useful for JIRA"
                                 plain (clock)
                                 "%?" :jump-to-captured t))
        org-agenda-custom-commands '(("n" "Agenda and all Todo's"
                                      ((agenda "")
                                       (alltodo "")))))

  ;; Keybindings
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ct" (lambda () (interactive) (org-capture nil "t")))
  (global-set-key "\C-cu" (lambda () (interactive) (org-capture nil "u"))))

(use-package org-src
  :ensure nil
  :after org
  :config
  (setq
   org-edit-src-content-indentation 0
   org-edit-src-persistent-message nil
   org-src-fontify-natively t
   org-src-preserve-indentation t
   org-src-tab-acts-natively t
   org-src-window-setup 'current-window))

(use-package org-jira
  :ensure nil
  :bind
  :config
  (setq jiralib-url "https://jira.itx.com")
  (global-set-key "\C-cj" #'org-jira-get-issue))

(provide 'my-org-config)
