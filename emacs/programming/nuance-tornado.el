(define-derived-mode tornado-template-expr-mode python-mode "Tornado Expr"
  (make-face 'tornado-variable-face)
  (font-lock-add-keywords
   nil
   '(
	 ;; keywords
	 (".*\\(extends\\|include\\|set\\|import\\|from\\|comment\\|apply\\|block\\|try\\|if\\|for\\|while\\).*" 1 font-lock-type-face))
  (font-lock-mode 1)))

(mmm-add-group
 'tornado-template
 '((tornado-expression
	:submode tornado-template-expr-mode
	:face mmm-code-submode
	:front "{%"
	:back "%}")
   (tornado-string
	:submode python-mode
	:face mmm-code-submode
	:front "{{"
	:back "}}")))

(mmm-add-mode-ext-class 'html-mode "\\.thtml\\'" 'tornado-template)

(provide 'nuance-tornado)
