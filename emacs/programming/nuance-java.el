 ;; Or enable more if you wish
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
								  global-semanticdb-minor-mode
								  global-semantic-idle-summary-mode
								  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

;; (require 'malabar-mode)

;; (setq malabar-groovy-lib-dir (concat dotfiles-dir "/elpa-to-submit/malabar-mode-1.4.0/lib"))
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

(require 'eclim)

(setq eclim-auto-save t)
(global-eclim-mode)

(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

(require 'ac-company)
(ac-company-define-source ac-source-company-eclim company-emacs-eclim)
(add-hook 'java-mode-hook
		  (lambda () 
			(add-to-list 'ac-sources 'ac-source-company-eclim)))

(provide 'nuance-java)

