;; find-file-in-project configuration file

(use-package find-file-in-project
  :ensure t
  :config
  (require 'find-file-in-project)
  (autoload 'find-file-in-project "find-file-in-project" nil t)
  (autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
  (autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
  (autoload 'ffip-show-diff "find-file-in-project" nil t)
  (autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
  (autoload 'ffip-ivy-resume "find-file-in-project" nil t)
  :bind
  ("C-c C-b" . find-file-in-project)) ; change this to :bind

(provide 'find-file-in-project.config)
