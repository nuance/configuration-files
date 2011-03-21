;;-----------------------------------------------------------------------------
;; Global keyBindings
;;-----------------------------------------------------------------------------
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key [f1] 'find-tag)
(global-set-key [f2] 'pop-tag-mark)
(global-set-key [f3] 'show-subtree)
(global-set-key [(shift f3)] 'show-all)
(global-set-key [f4] 'hide-subtree)
(global-set-key [(shift f4)] 'hide-sublevels)
(global-set-key [f5] 'flymake-goto-prev-error)
(global-set-key [f6] 'flymake-goto-next-error)
(global-set-key [f7] 'flymake-start-syntax-check)
(global-set-key [f8] 'open-diary)
(global-set-key [C-f8] 'save-diary)

(global-set-key [(control ^)] 'show-subtree)
(global-set-key [(control meta ^)] 'hide-subtree)

;; Go to binding
(require 'nuance-defuns)
(global-set-key "\M-g" 'smarter-goto-line)

;; Smarter centering
(global-set-key [(control l)]  'centerer)

;; Default to regexp search
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)

;; Fullscreen
(global-set-key [(super return)] 'ns-toggle-fullscreen)


(global-set-key "\M-R" 'org-remember)

(provide 'nuance-bindings)
