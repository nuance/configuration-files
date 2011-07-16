;;-----------------------------------------------------------------------------
;; Color theme
;;-----------------------------------------------------------------------------

(require 'color-theme)
(color-theme-initialize)
(load-library "color-theme-twilight")
(load-library "color-theme-solarized")

(color-theme-twilight)

(defun dark () 
  (interactive)
  (color-theme-twilight))

(defun light () 
  (interactive)
  (color-theme-solarized-light))

(provide 'nuance-colortheme)
