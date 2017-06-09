;; |==============================================|
;; |  title: color-configuration.el               |
;; |  description: configure custom colors        |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; FIX ME PLEASE!

;; color list
(defconst custom-color-selection "#373b41") ;; remove this, use gray
(defconst custom-color-gray "#373b41")
(defconst custom-color-background "#1d1f21") ;; remove this, use black
(defconst custom-color-black "#1d1f21")
(defconst custom-color-current "#282a2e")
(defconst custom-color-red "#cc66666")
(defconst custom-color-orange "#de935f")
(defconst custom-color-yellow "#f0c674")
(defconst custom-color-green "#b5bd68")
(defconst custom-color-aqua "#8abeb7")
(defconst custom-color-blue "#81a2be")
(defconst custom-color-purple "#b294bb")

(defconst colors/black "#1d1f21")
(defconst colors/blue "#81a2be")
(defconst colors/green "#b5bd68")
(defconst colors/red "#cc6666")
(defconst colors/yellow "#f0c674")

;; git-gutter
(defconst git-gutter/added--background colors/green)
(defconst git-gutter/deleted--background colors/red)
(defconst git-gutter/modified--background colors/yellow)

;; helm
(defconst helm/selection--background nil)
(defconst helm/selection--foreground colors/blue)
(defconst helm/source-header--background nil)
(defconst helm/source-header--foreground colors/blue)
(defconst helm/source-header--height 1.5)

;; editor cursor
(defconst custom-editor-cursor-color custom-color-blue)

;; editor line highlight
(defconst custom-background-editor-highlight-color custom-color-current)
(defconst custom-foreground-editor-highlight-color nil)

;; editor region selection
(defconst custom-background-editor-region-color custom-color-blue)
(defconst custom-foreground-editor-region-color custom-color-background)

(provide 'color-configuration)
