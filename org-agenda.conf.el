(setq org-agenda-category-icon-alist
      '(("[Ee]macs" "/usr/share/icons/hicolor/16x16/apps/emacs.png" nil nil :ascent center)
        ("\\(Party\\|Celeb\\)" "~/.emacs.d/icons/org/party.png" nil nil :ascent center)
        ("Gnus" "~/.emacs.d/icons/org/gnus.png" nil nil :ascent center)
        ("Org" "~/.emacs.d/icons/org/org.png" nil nil :ascent center)
        ("Music" "~/.emacs.d/icons/org/music.png" nil nil :ascent center)
        ("Work" "~/.emacs.d/icons/org/work.png" nil nil :ascent center)
        ("Personal" "~/.emacs.d/icons/org/personal.png" nil nil :ascent center)
        ("Tasks" "~/.emacs.d/icons/org/tasks.png" nil nil :ascent center)
        ("Anniversary" "~/.emacs.d/icons/org/anniversary.png" nil nil :ascent center)
        ("Linux" "~/.emacs.d/icons/org/computer.png" nil nil :ascent center)
        ("Books" "~/.emacs.d/icons/org/book.png" nil nil :ascent center)
        ("\\(Holidays\\|Vacation\\)" "~/.emacs.d/icons/org/holidays.png" nil nil :ascent center)
        (".*" '(space . (:width (16))))))

(setq org-agenda-files (list nemacs-org-inbox-file)
      org-agenda-start-on-weekday 0)

(add-hook 'org-agenda-mode-hook #'hl-line-mode)
