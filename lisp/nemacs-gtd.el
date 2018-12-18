;;; nemacs-gtd.el --- Getting Things Done on Emacs.

;; Copyright (C) 2017 ~ 2018 Nahuel Jesús Sacchetti <nahueljsacchetti@gmail.com>

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-bullets)
(require 'org-id)
(require 'org-super-agenda)
(require 'org-gcal.conf)

;; Functions
(defun nemacs-get-inbox-file ()
  "Open my Inbox file."
  (interactive)
  (find-file nemacs-org-inbox-file))

(defun nemacs-get-todo-file ()
  "Open my TODO file."
  (interactive)
  (find-file nemacs-org-todo-file))

(defun nemacs-get-org-file (filename)
  "Get the Org notes file that's shared between different devices. Concat the `filename' with the directory"
  (expand-file-name filename nemacs-notes-dir))

(defun nemacs-capture-todo ()
  "Creates new TODO capture"
  (interactive)
  (org-capture :keys "t"))

(defun nemacs-new-daily-review ()
  "Creates a new daily review entry."
  (interactive)
  (progn
    (org-capture nil "d")
    (org-capture-finalize t)
    (org-speed-move-safe 'outline-up-heading)
    (org-narrow-to-subtree)
    (org-gcal-fetch)
    (org-clock-in)))

(defun nemacs-new-weekly-review ()
  "Creates a new weekly review entry."
  (interactive)
  (progn
    (org-capture nil "w")
    (org-capture-finalize t)
    (org-speed-move-safe 'outline-up-heading)
    (org-narrow-to-subtree)
    (org-gcal-fetch)
    (org-clock-in)))

;; Files
(setq nemacs-org-archive-file (nemacs-get-org-file "archive.org")
      nemacs-org-inbox-file (nemacs-get-org-file "inbox.org")
      nemacs-org-someday-file (nemacs-get-org-file "someday.org")
      nemacs-org-todo-file (nemacs-get-org-file "todo.org")
      nemacs-calendar-dir (concat nemacs-notes-dir "calendar"))

(setq nemacs-agenda-files `(,nemacs-org-inbox-file ,nemacs-org-someday-file ,nemacs-org-todo-file))
(dolist (file (directory-files nemacs-calendar-dir))
  (when (string-match (format "^\\(.+\\)\\.org$") file)
    (setq org-file-found (expand-file-name file nemacs-calendar-dir))
    (add-to-list 'nemacs-agenda-files org-file-found)))

;; Hooks
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-agenda-mode-hook #'hl-line-mode)
(add-hook 'org-agenda-mode-hook (lambda () (setq line-spacing 0.2)))
(add-hook 'org-after-refile-insert-hook #'save-buffer)
(add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-capture-before-finalize-hook
          (lambda ()
            (interactive)
            (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
            (org-id-get-create)))
(add-hook 'org-mode-hook
          (lambda ()
             (setq line-spacing 0.2)
             (variable-pitch-mode 1)))

;; Org ID configuration
(setq org-id-locations-file (concat nemacs-notes-dir "references/org-id-db")
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Defaults
(setq org-agenda-category-icon-alist '(("Calendar" "~/.emacs.d/icons/org/calendar.png" nil nil :ascent center)
                                       ("Emacs" "~/.emacs.d/icons/org/emacs.png" nil nil :ascent center)
                                       ("Inbox" "~/.emacs.d/icons/org/inbox.png" nil nil :ascent center)
                                       ("Life" "~/.emacs.d/icons/org/qol.png" nil nil :ascent center)
                                       ("Personal" "~/.emacs.d/icons/org/personal.png" nil nil :ascent center)
                                       ("Someday" "~/.emacs.d/icons/org/someday.png" nil nil :ascent center)
                                       ("Work" "~/.emacs.d/icons/org/itx.png" nil nil :ascent center)

                                       (".*" '(space . (:width (16)))))
      org-agenda-files nemacs-agenda-files
      org-agenda-start-on-weekday 0
      org-archive-location (concat nemacs-org-archive-file "::* From %s")
      org-clock-in-switch-to-state "STARTED"
      org-clock-in-resume t
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-persist t
      org-clock-persist-query-resume nil
      org-deadline-warning-days 7
      org-default-notes-file nemacs-org-inbox-file
      org-descriptive-links t
      org-directory nemacs-notes-dir
      org-edit-src-content-indentation 0
      org-edit-src-persistent-message nil
      org-ellipsis "\u21b4"
      org-email-link-description-format "Email %c: %s"
      org-enforce-todo-dependencies t
      org-fontify-done-headline t
      org-fontify-whole-heading-line t
      org-image-actual-width nil
      org-log-done 'time
      org-log-into-drawer t
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-startup-folded t
      org-startup-truncated nil
      org-support-shift-select 'always
      org-tags-column 0)

;; Capture
(setq org-todo-keywords '((sequence "TODO(t!)"
                                    "NEXT(n@)"
                                    "STARTED"
                                    "|"
                                    "DONE(d)"
                                    "CANCELED(c@)"))
      org-capture-templates '(("t" "Add a new TODO entry"
                               entry (file nemacs-org-inbox-file)
                               "* TODO %?" :kill-buffer t)
                              ("d" "Daily Review"
                               entry (file+olp+datetree "/tmp/reviews.org")
                               (file "~/Dropbox/orgfiles/templates/daily-review.template.org"))
                              ("w" "Weekly Review"
                               entry (file+olp+datetree "/tmp/reviews.org")
                               (file "~/Dropbox/orgfiles/templates/weekly-review.template.org")))
      org-tag-persistent-alist '(;; Context
                                 ("@errand"   . ?e)
                                 ("@home"     . ?h)
                                 ("@office"   . ?o)

                                 ;; Time
                                 ("Today"     . ?=)
                                 ("Tomorrow"  . ?>)
                                 ("This Week" . ?7)

                                 ;; Focus
                                 ("IMMERSIVE" . ?I)
                                 ("PROCESS"   . ?P)))

;; Refile
(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets `(((,nemacs-org-inbox-file ,nemacs-org-someday-file ,nemacs-org-todo-file) :maxlevel . 3)))

;; Agenda
(org-super-agenda-mode)

(setq org-agenda-inhibit-startup nil
      org-agenda-show-future-repeats nil
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-if-done nil
      org-agenda-skip-scheduled-if-done nil)

(setq org-agenda-time-grid '((daily today require-timed remove-match)
                             (700 800 900 1000 1100 1200 1300 1400 1500 1600 1800 2000)
                             "......" "----------------"))

(setq org-agenda-custom-commands `(("r" "Daily Review"
                                    ((tags-todo "Today")
                                     (tags-todo "Tomorrow")
                                     (agenda "" ((org-agenda-span 2)
                                                 (org-deadline-warning-days 7)
                                                 (org-agenda-start-on-weekday nil)))))
                                   ("g" . "Getting Things Done")
                                   ("go" "at Office"
                                    ((tags-todo "@office")
                                     (agenda "" ((org-agenda-span 1)
                                                 (org-deadline-warning-days 7)
                                                 (org-agenda-start-on-weekday nil)))
                                     (tags-todo "This Week")))))

;; Keybindings
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'nemacs-capture-todo)
(global-set-key (kbd "C-c i") #'nemacs-get-inbox-file)
(global-set-key (kbd "C-c t") #'nemacs-get-todo-file)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c r d") #'nemacs-new-daily-review)
(global-set-key (kbd "C-c r w") #'nemacs-new-weekly-review)
(define-key org-agenda-mode-map "g" #'org-gcal-fetch)

;; UI
(zenburn-with-color-variables
  (custom-set-faces
   `(org-agenda-date ((t (:foreground ,zenburn-fg+1 :underline t))))
   `(org-agenda-date-today ((t (:foreground ,zenburn-fg+1 :height 1.2 :underline t :inherit org-agenda-date))))
   `(org-agenda-date-weekend ((t (:foreground ,zenburn-fg-1 :inherit org-agenda-date))))
   `(org-agenda-structure ((t (:foreground ,zenburn-fg :weight bold))))
   `(org-agenda-calendar-event ((t (:foreground ,zenburn-yellow))))
   `(org-time-grid ((t (:foreground ,zenburn-bg+3))))
   `(org-level-1 ((t (:foreground unspecified :weight normal))))
   `(org-level-2 ((t (:foreground ,zenburn-yellow-1))))
   `(org-priority ((t (:foreground ,zenburn-cyan))))
   `(org-property ((t (:foreground ,zenburn-green :weight bold))))
   `(org-property-value ((t (:foreground ,zenburn-green+4 :italic t))))
   `(org-special-keyword ((t (:foreground ,zenburn-green))))
   `(org-tag ((t (:foreground ,zenburn-blue))))
   `(org-todo ((t (:foreground ,zenburn-red-2 :underline nil :weight bold))))
   `(org-done ((t (:foreground ,zenburn-green-1 :underline t :weight bold)))))

  (setq org-todo-keyword-faces
        `(("TODO" . org-todo)
          ("STARTED" . (:foreground ,zenburn-green :weight bold))
          ("NEXT" . (:foreground ,zenburn-blue-2 :underline nil :weight bold))
          ("DONE" . org-done)
          ("CANCELED" . (:foreground ,zenburn-red :underline t :weight bold)))))

(provide 'nemacs-gtd)
