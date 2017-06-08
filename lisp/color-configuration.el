;; |==============================================|
;; |  title: color-configuration.el               |
;; |  description: configure custom colors        |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; color list
(defconst custom-color-selection "#373b41") ;; remove this, use gray
(defconst custom-color-gray "#373b41")
(defconst custom-color-background "#1d1f21")
(defconst custom-color-current "#282a2e")
(defconst custom-color-red "#cc66666")
(defconst custom-color-orange "#de935f")
(defconst custom-color-yellow "#f0c674")
(defconst custom-color-green "#b5bd68")
(defconst custom-color-aqua "#8abeb7")
(defconst custom-color-blue "#81a2be")
(defconst custom-color-purple "#b294bb")

(defun face (face &rest spec)
  (face-spec-set face (list (cons t spec))))

;; spaceline
(face 'spaceline/read-only t
      :background custom-color-blue)
(face 'spaceline/modified nil
      :background custom-color-red
      :foreground "black")
(face 'spaceline/normal t
      :background custom-color-green
      :foreground custom-color-background)

;; helm
(face 'helm/selection nil
      :background custom-color-gray
      :foreground custom-color-blue
      :bold t)
(face 'helm/source-header nil
      :background nil
      :foreground custom-color-blue
      :height 1.5
      :box nil)
(face 'helm/action nil
      :underline nil)
(face 'helm/action nil
      :background nil)

;; editor cursor
(defconst custom-editor-cursor-color custom-color-blue)

;; git gutter
(defconst custom-git-modified-color custom-color-yellow)
(defconst custom-git-added-color custom-color-green)
(defconst custom-git-deleted-color custom-color-red)

;; menu selections
(defconst custom-foreground-menu-selection-color custom-color-blue)
(defconst custom-background-menu-selection-color custom-color-selection)

;; menu headers
(defconst custom-foreground-menu-header-color custom-color-blue)
(defconst custom-background-menu-header-color nil)

;; editor line highlight
(defconst custom-background-editor-highlight-color custom-color-current)
(defconst custom-foreground-editor-highlight-color nil)

;; editor region selection
(defconst custom-background-editor-region-color custom-color-blue)
(defconst custom-foreground-editor-region-color custom-color-background)

(provide 'color-configuration)
