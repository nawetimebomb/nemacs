(setq nemacs-agenda-files '()
      nemacs-projects-dir (concat nemacs-notes-dir "/projects"))

(add-to-list 'nemacs-agenda-files (expand-file-name "inbox.org" nemacs-notes-dir))

(setq org-agenda-category-icon-alist
      '(("Inbox" "~/.emacs.d/icons/org/inbox.png" nil nil :ascent center)
        ("Emacs" "~/.emacs.d/icons/org/emacs.png" nil nil :ascent center)
        ("Quality of Life" "~/.emacs.d/icons/org/qol.png" nil nil :ascent center)
        ("Someday" "~/.emacs.d/icons/org/someday.png" nil nil :ascent center)
        ("Work" "~/.emacs.d/icons/org/work.png" nil nil :ascent center)
        (".*" '(space . (:width (16))))))

(dolist (file (directory-files nemacs-projects-dir))
  (when (string-match (format "^\\(.+\\)\\.org$") file)
    (setq org-file-found (expand-file-name file nemacs-projects-dir))
    (add-to-list 'nemacs-agenda-files org-file-found)
    (add-to-list 'org-refile-targets `(,org-file-found :level . 0))))

(setq org-agenda-files nemacs-agenda-files
      org-agenda-start-on-weekday 0)

(add-hook 'org-agenda-mode-hook #'hl-line-mode)
