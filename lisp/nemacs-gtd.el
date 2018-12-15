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
(require 'org-gcal.conf)

;; Functions
(defun nemacs-get-inbox-file ()
  "Open my Inbox file."
  (interactive)
  (find-file nemacs-org-inbox-file))

(defun nemacs-get-org-file (filename)
  "Get the Org notes file that's shared between different devices. Concat the `filename' with the directory"
  (expand-file-name filename nemacs-notes-dir))

(defun nemacs-capture-todo ()
  "Creates new TODO capture"
  (interactive)
  (org-capture :keys "t"))

;; Files
(setq nemacs-agenda-files '()
      nemacs-org-archive-file (nemacs-get-org-file "archive.org")
      nemacs-org-inbox-file (nemacs-get-org-file "inbox.org")
      nemacs-calendar-dir (concat nemacs-notes-dir "calendar")
      nemacs-projects-dir (concat nemacs-notes-dir "projects"))

(add-to-list 'nemacs-agenda-files (expand-file-name "inbox.org" nemacs-notes-dir))
(dolist (file (directory-files nemacs-projects-dir))
  (when (string-match (format "^\\(.+\\)\\.org$") file)
    (setq org-file-found (expand-file-name file nemacs-projects-dir))
    (add-to-list 'nemacs-agenda-files org-file-found)
    (add-to-list 'org-refile-targets `(,org-file-found :level . 0))))
(dolist (file (directory-files nemacs-calendar-dir))
  (when (string-match (format "^\\(.+\\)\\.org$") file)
    (setq org-file-found (expand-file-name file nemacs-calendar-dir))
    (add-to-list 'nemacs-agenda-files org-file-found)
    (add-to-list 'org-refile-targets `(,org-file-found :level . 0))))

;; Hooks
(add-hook 'org-agenda-mode-hook #'hl-line-mode)
(add-hook 'org-after-refile-insert-hook #'save-buffer)
(add-hook 'org-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook #'hl-line-mode)
(add-hook 'org-capture-before-finalize-hook
          (lambda ()
            (interactive)
            (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
            (org-id-get-create)))

;; Org ID configuration
;; TODO: Needs work on saving the database of ids.
(require 'org-id)
(setq org-id-locations-file (concat nemacs-notes-dir "references/org-id-db")
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Defaults
(setq org-agenda-category-icon-alist '(("Inbox" "~/.emacs.d/icons/org/inbox.png" nil nil :ascent center)
                                       ("Emacs" "~/.emacs.d/icons/org/emacs.png" nil nil :ascent center)
                                       ("Life" "~/.emacs.d/icons/org/qol.png" nil nil :ascent center)
                                       ("Someday" "~/.emacs.d/icons/org/someday.png" nil nil :ascent center)
                                       ("Work" "~/.emacs.d/icons/org/work.png" nil nil :ascent center)
                                       ("Calendar" "~/.emacs.d/icons/org/calendar.png" nil nil :ascent center)
                                       (".*" '(space . (:width (16)))))
      org-agenda-files nemacs-agenda-files
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
                                    "|"
                                    "DONE(d)"
                                    "CANCELED(c@)"))
      org-capture-templates '(("t" "Add a new TODO entry"
                               entry (file nemacs-org-inbox-file)
                               "* TODO %?" :kill-buffer t)
                              ("d" "Daily Review"
                               entry (file+olp+datetree "/tmp/reviews.org")
                               (file "~/Dropbox/orgfiles/templates/daily-review.template.org")))
      org-tag-persistent-alist '(("computer"  . ?c)
                                 ("finances"  . ?f)
                                 ("goals"     . ?g)
                                 ("home"      . ?h)
                                 ("phone"     . ?p)
                                 ("office"    . ?o)
                                 ("weekend"   . ?w)))

;; Refile
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)

;; Custom Agenda Commands
(setq org-agenda-custom-commands '(("g" . "Getting Things Done")
                                    ("go" "At Office"
                                    ((tags-todo "+office|+computer-weekend")
                                     (agenda #1="")))))

;; Keybindings
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'nemacs-capture-todo)
(global-set-key (kbd "C-c i") #'nemacs-get-inbox-file)
(global-set-key (kbd "C-c l") #'org-store-link)
(define-key org-agenda-mode-map "g" #'org-gcal-fetch)

;; UI
(zenburn-with-color-variables
  (custom-set-faces
   `(org-agenda-date ((t (:foreground ,zenburn-blue+1))))
   `(org-agenda-date-today ((t (:foreground ,zenburn-blue+1 :height 1.2 :underline nil :inherit org-agenda-date))))
   `(org-agenda-date-weekend ((t (:foreground ,zenburn-bg+3 :inherit org-agenda-date))))
   `(org-agenda-structure ((t (:foreground ,zenburn-fg :weight bold))))
   `(org-agenda-calendar-event ((t (:foreground ,zenburn-fg))))
   `(org-time-grid ((t (:foreground ,zenburn-fg-1))))
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
          ("NEXT" . (:foreground ,zenburn-blue-2 :underline nil :weight bold))
          ("DONE" . org-done)
          ("CANCELED" . (:foreground ,zenburn-red :underline t :weight bold)))))

(provide 'nemacs-gtd)
