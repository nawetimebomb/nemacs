(use-package org-roam
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   (:map org-mode-map
         ("C-M-i" . completion-at-point))
   (:map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("D" . org-roam-dailies-capture-today)
         ("T" . org-roam-dailies-capture-tomorrow)))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-directory "~/Notes/Roam")
  (org-roam-dailies-directory "Journal/"))
