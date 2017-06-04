;; |==============================================|
;; |  title: color-configuration.el               |
;; |  description: configure custom colors        |
;; |  copyright: elnawe.com (c) 2017              |
;; |==============================================|

;; color list
(defconst custom-color-selection "#373b41")
(defconst custom-color-background "#1d1f21")
(defconst custom-color-current "#282a2e")
(defconst custom-color-red "#cc66666")
(defconst custom-color-orange "#de935f")
(defconst custom-color-yellow "#f0c674")
(defconst custom-color-green "#b5bd68")
(defconst custom-color-aqua "#8abeb7")
(defconst custom-color-blue "#81a2be")
(defconst custom-color-purple "#b294bb")

;; editor cursor
(defconst custom-editor-cursor-color custom-color-green)

;; menu selections
(defconst custom-foreground-menu-selection-color custom-color-green)
(defconst custom-background-menu-selection-color custom-color-selection)

;; menu headers
(defconst custom-foreground-menu-header-color custom-color-green)
(defconst custom-background-menu-header-color nil)

;; editor line highlight
(defconst custom-background-editor-highlight-color custom-color-current)
(defconst custom-foreground-editor-highlight-color nil)

;; editor region selection
(defconst custom-background-editor-region-color custom-color-green)
(defconst custom-foreground-editor-region-color custom-color-background)

(provide 'color-configuration)
