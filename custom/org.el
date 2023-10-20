(use-package org
  :straight org-contrib
  :preface
  (defun nemacs-setup-org-mode ()
    (setq-local line-spacing 0.1))
  :hook
  (org-mode                . nemacs-setup-org-mode)
  (org-after-refile-insert . org-save-all-org-buffers)
  :bind
  (("C-c l" . org-store-link)
   ("C-c t" . org-todo))
  :custom
  (org-adapt-indentation nil)
  (org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
  (org-deadline-warning-days 3)
  (org-descriptive-links t)
  (org-ellipsis "↓")
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-link-frame-setup '((file . find-file)))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  (org-persist-directory (concat nemacs-cache-dir "org-persist/"))
  (org-read-date-prefer-future 'time)
  (org-return-follows-link t)
  (org-startup-folded nil)
  (org-startup-truncated nil)
  (org-tags-column -70)
  (org-todo-keywords '((sequence "TODO(t!)" "PROJECT(p)" "NEXT(n!)" "WAITING(w!)" "DONE(d!)"))))

(use-package org-capture
  :straight nil
  :bind
  (("s-o" . org-capture))
  :custom
  (org-capture-templates
   '(("i" "Entrada de Inbox"
      entry (file "~/Notes/inbox.org") "* TODO %?\n%T")
     ("c" "Entrada para Cuento de la Buena Pipa"
      entry (file "~/Notes/cdlbp.org") "* TODO %?\n%T")
     ("p" "Entrada para Privacidad"
      entry (file "~/Notes/privacy.org") "* TODO %?\n%T"))))

(use-package org-agenda
  :straight nil
  :preface
  (defun nemacs-org-agenda-hook ()
    (hl-line-mode)
    (setq line-spacing 0.2))
  :bind
  (("C-c a" . org-agenda))
  :hook
  (org-agenda-mode . nemacs-org-agenda-hook)
  :custom
  (org-agenda-archives-mode nil)
  (org-agenda-files '("~/Notes/inbox.org"
                      "~/Notes/cdlbp.org"
                      "~/Notes/privacy.org"))
  (org-agenda-inhibit-startup nil)
  (org-agenda-show-future-repeats t)
  (org-agenda-skip-deadline-if-done nil)
  (org-agenda-skip-scheduled-if-done nil)
  (org-agenda-start-on-weekday 0))
