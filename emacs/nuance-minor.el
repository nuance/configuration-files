;;-----------------------------------------------------------------------------
;; Minor modes
;;-----------------------------------------------------------------------------

;; Yelp customizations
(require 'yelp)

;; Electric pairs
(require 'pair-mode)

;; Cheat
(require 'cheat)

;; Line numbering - M-x linum
(require 'linum)

;; Line highlighting
(require 'highline)

;; Textmate mode
;;(require 'textmate)
;;(global-set-key "\C-q" 'textmate-goto-file)

;; (require 'pomodoro)

(require 'nuance-diary)

(require 'rainbow-mode)

(load-library "flymake-cursor")

(add-to-list 'load-path "~/.emacs.d/elpa-to-submit/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa-to-submit/auto-complete/ac-dict")
(ac-config-default)

(provide 'nuance-minor)
