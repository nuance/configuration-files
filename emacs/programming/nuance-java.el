(require 'ac-company)
(ac-company-define-source ac-source-company-elisp company-elisp)
(add-hook 'java-mode-hook
		  (lambda () 
			(add-to-list 'ac-sources 'ac-source-company-eclim)))