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
(global-set-key [f12] 'open-diary)
(global-set-key [C-f12] 'save-diary)

;; Go to binding
(global-set-key "\M-g" 'goto-line)

;; Smarter centering
(global-set-key [(control l)]  'centerer)

;; Default to regexp search
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)

(provide 'nuance-bindings)
