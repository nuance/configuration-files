;;-----------------------------------------------------------------------------
;; Python mode
;;-----------------------------------------------------------------------------

(load-library "python")

(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook '(lambda () (custom-set-variables
                                         '(py-indent-offset 4)
                                         '(tab-width 4)
                                         '(py-smart-indentation nil))))
                                         
(defun python-auto-fill-comments-only ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))

(add-hook 'python-mode-hook
          (lambda ()
            (python-auto-fill-comments-only)))

;;(add-hook 'python-mode-hook
;;		  '(lambda () 
;;			 (eldoc-mode 1)
;;			 (set (make-local-variable 'eldoc-documentation-function)
;;				  'py-eldoc-function) t)

(defun py-eldoc-function (&optional word)
  "Launch PyDOC on the Word at Point"
  (interactive)
  (let* ((word (if word word (thing-at-point 'symbol)))
		 (docs (shell-command-to-string (concat "/usr/bin/python" " -c \"from pydoc import help;help(\'" word "\')\"")))
		 (split-docs (split-string docs "|  ")))
	(if (> (length split-docs) 1) (car (split-string (cadr split-docs) "\n")) nil)))

;; Outline mode
;; setup python mode
                                        ; add my customization
(add-hook 'python-mode-hook 'my-python-hook)
                                        ; this gets called by outline to deteremine the level. Just use the length of the whitespace
(defun py-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))
                                        ; this get called after python mode is enabled
(defun my-python-hook ()
                                        ; outline uses this regexp to find headers. I match lines with indented "class"
                                        ; and "def" lines and lines with @ (attribute)
  (setq outline-regexp "[ \t]*\\(def\\|class\\)\\|[\t]*\\@[A-Za-z]+\\|[A-Za-z]+")
                                        ; enable our level computation
  (setq outline-level 'py-outline-level)
                                        ; turn on outline mode
  (outline-minor-mode t)
                                        ; initially hide all but the headers
  (hide-body)
                                        ; make paren matches visible
  (show-paren-mode 1)
                                        ; higlight current line
  ;;  (highline-mode)
  )

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))

(define-derived-mode cython-mode python-mode "Cython"
  (make-face 'cython-variable-face)
  (font-lock-add-keywords
   nil
   '(
     ("\\<\\(NULL\\|c\\(def\\|har\\|typedef\\)\\|double\\|e\\(num\\|xtern\\)\\|float\\|in\\(clude|t\\)\\|object\\|public\\|readonly\\|struct\\|type\\|union\\|void\\)\\>" 1 font-lock-builtin-face)
     )
   )
  (font-lock-mode 1)
  (setq outline-regexp "[ \t]*\\(def\\|class\\|cdef\\)[^:]*:\\|[ \t]*\\@")
  (setq outline-level 'py-outline-level)
  (outline-minor-mode t)
  (hide-body)
  )

;; (require 'hide-lines)

;; (defun mr-focus ()
;;   (interactive)
;;   (hide-non-matching-lines "\\(yield\\|def\\|class\\)"))

(add-hook 'python-mode-hook 
		  '(lambda () (define-key python-mode-map "\C-cm" 'mr-focus)))

(provide 'nuance-python)
