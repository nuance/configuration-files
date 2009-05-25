;;-----------------------------------------------------------------------------
;; Mako mode (MMM derived mode)
;;-----------------------------------------------------------------------------

(require 'mmm-mako)
(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)

;;-----------------------------------------------------------------------------
;; Cheetah mode - mix of html-mode and a basic cheetah keyword highlighter
;;-----------------------------------------------------------------------------

(define-derived-mode cheetah-mode html-mode "Cheetah"
  (make-face 'cheetah-variable-face)
  (font-lock-add-keywords
   nil
   '(
     ;; #(symbol) lines
     ("\\(#\\(attr\\|def\\|block\\|from\\|else\\|else if\\|include\\|extends\\|implements\\|set\\|import\\|for\\|if\\|end\\|block\\)\\)\\>" 1 font-lock-type-face)
     ;; PSP-style
     ("\\(#\\(from\\|for\\|end\\)\\).*\\<\\(for\\|import\\|def\\|if\\|in\\)\\>" 3 font-lock-type-face)
     ;; One-line #if EXPR1 then EXPR2 else EXPR3(# or eol)
     ("#if[^$#]*then[^$#]*else[^$#]*\\(#\\|$\\)" 1 font-lock-builtin-face)
     ;; Comments
     ("\\(^##.*\\)\n" 1 font-lock-comment-face)
     ;; $var
     ("\\(\\$\\(?:\\sw\\|}\\|{\\|\\s_\\)+\\)" 1 font-lock-variable-name-face)
     ;; #set var
     ()
     ;; Methods
     ("^#.*\\(True\\|False\\|None\\)" 1 font-lock-builtin-face)))
  (font-lock-mode 1))

;;-----------------------------------------------------------------------------
;; Smart Cheetah (uses mmm for javascript, css)
;;-----------------------------------------------------------------------------

;; CSS-Mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)

;; javascript-generic-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(autoload 'javascript-mode "javascript" nil t)

;; Set up an mmm group for fancy html editing
(mmm-add-group
 'fancy-cheetah
 '(
   (html-cheetah-portion
    :submode cheetah-mode
    :face mmm-code-submode-face
    :front "#"
	:back "$")
   (html-css-embedded
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "<style\[^>\]*>"
    :back "</style>")
   (html-css-attribute
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "\\bstyle=\\s-*\""
    :back "\"")
   (html-script-embedded
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script>"
    :back "</script>")
   (html-javascript-embedded
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script type=\"text/javascript\">"
    :back "</script>")
   (html-javascript-attribute
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "\\bon\\w+=\\s-*\""
    :back "\"")))

(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-cheetah))

(setq auto-mode-alist (cons '( "\\.tmpl\\'" . html-mode ) auto-mode-alist ))

(provide 'nuance-templating)
