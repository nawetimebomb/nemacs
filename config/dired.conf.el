(use-package dired
  :ensure nil
  :custom
  (dired-open-extensions '(("pdf" . "evince")
                              ("mkv" . "vlc")
                              ("mp4" . "vlc")
                              ("avi" . "vlc"))))
