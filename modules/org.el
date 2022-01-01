(use-package org
  :straight org-contrib
  :preface
  (defun nemacs-setup-org-mode ()
    (setq-local line-spacing 0.1))
  :hook
  (org-mode                . nemacs-setup-org-mode)
  (org-after-refile-insert . org-save-all-org-buffers)
  :bind
  (("C-c a"     . org-agenda)
   ("C-c l"     . org-store-link)
   ("C-c C-o g" . org-clock-goto))
  :custom
  (org-adapt-indentation nil)
  (org-blank-before-new-entry '((heading . t)
                                (plain-list-item . nil)))
  (org-deadline-warning-days 3)
  (org-descriptive-links t)
  (org-ellipsis " ↓")
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-link-frame-setup '((file . find-file)))
  (org-log-done 'time)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  (org-read-date-prefer-future 'time)
  (org-return-follows-link t)
  (org-startup-folded nil)
  (org-startup-truncated nil)
  (org-support-shift-select 'always)
  (org-persist-directory (concat nemacs-cache-dir "org-persist/"))
  (org-tags-column -115))
