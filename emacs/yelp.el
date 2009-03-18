;;-----------------------------------------------------------------------------
;; Yelp helper code
;;-----------------------------------------------------------------------------

;;=====================================
;; Helper functions
;;=====================================

;; at each step pull off top directory and prepend it to file-relative
;; return when the next directory down is named pg or nil
(defun split-branch-h (base parent relative)
  (cond
   ;; Found the pg dir as parent, so return our arguments
   ((string-equal (file-name-nondirectory (directory-file-name (file-name-directory parent))) base)
    (list parent relative))
   ;; Found /, so return nil
   ((string-equal parent "/") nil)
   ;; Otherwise, recurse
   (t (split-branch-h base (directory-file-name (file-name-directory parent))
                      (concat (file-name-nondirectory parent) "/" relative)))))

;; return the branch folder (eg ~/pg/loc/) and the relative path
;; so (current-branch .../pg/happy_faces/logic/reviews.py) will return '(.../pg/happy_faces logic/reviews.py)
(defun split-branch (base &optional filename)
  (let ((current-file (if filename filename (buffer-file-name))))
    (if current-file
        (split-branch-h base (file-name-directory current-file) (file-name-nondirectory current-file))
      nil)))

(defun current-branch (&optional base filename)
  (if (not base) (car (split-branch "pg" filename))
	(car (split-branch base filename))))

(defun current-branchname (&optional base filename)
  (let* ((bse (if base base "pg"))
		 (branch (car (split-branch "pg" filename))))
	(if branch (file-name-nondirectory branch) nil)))

;;=====================================
;; File navigation
;;=====================================

;; Default open to playground
(defun open-from-pg ()
  (interactive)
  (ido-find-file-in-dir "/media/pg/"))

;; Switch to tests / switch from tests
;; Basic idea - find the current branch, append tests to the relative dir,
;; and modify the file name from (.*)\.py to $1-test.py
(defun test-file-name (file-name)
  (if (split-branch "tests")
      ;; It's a test, so just return the file
      file-name
    ;; find the correct file
    (let* ((buffer-file (split-branch "pg" file-name))
           (current-branch (car buffer-file))
           (current-file (cadr buffer-file)))
      (concat current-branch "/tests/" (file-name-sans-extension current-file)
              "_test" (file-name-extension current-file t)))))

(defun nontest-file-name (file-name)
  (if (not (split-branch "tests"))
      file-name
    (let* ((buffer-file (split-branch "tests" file-name))
           (folder (file-name-nondirectory (directory-file-name (car buffer-file))))
           (current-branch (file-name-directory (directory-file-name (file-name-directory (directory-file-name (car buffer-file))))))
           (current-file (cadr buffer-file)))
      (concat current-branch folder (substring (file-name-sans-extension current-file) 0 -5)
              (file-name-extension current-file t)))))

(defun switch-to-test ()
  (find-file (test-file-name (buffer-file-name))))

(defun switch-from-test ()
  (find-file (nontest-file-name (buffer-file-name))))

(defun toggle-test-file ()
  (interactive)
  (if (split-branch "tests")
      (switch-from-test)
    (switch-to-test)))

;;=====================================
;; Playground commands
;;=====================================

;; Switch loc
(defun switch-loc ()
  (interactive)
  (let* ((branch (current-branch "pg"))
		 (pg (file-name-directory (directory-file-name branch))))
	(message (format "cd %s && rm loc" pg))
	(message (format "ln -s %s %sloc" branch pg))))

;; Make templates
(defun make-branch (&optional branch)
  (interactive)
  (let ((branch (if branch branch (current-branch "pg"))))
	(cd branch)
	(compile "make")))

;;=====================================
;; Misc
;;=====================================

(defun yelp-local-name (filename)
   ;; Found / on a tramp connection
   (if (tramp-tramp-file-p filename) 
	   (tramp-file-name-localname (tramp-dissect-file-name filename))
	 filename))

;; Open current document in firefox (should work for static, templates & exposed servlet methods)

;; Run current test
(defun run-test ()
  (interactive)
  (cd (current-branch))
  (compile (format "PYTHONPATH=%s python %s" (yelp-local-name (current-branch))
				   (yelp-local-name (test-file-name (buffer-file-name))))))

;; Graceful apache
(defun apache-graceful ()
  (interactive)
  (shell-command "myapachectl -k graceful"))

;; Start/Stop lucene

(define-minor-mode yelp-mode
  "Yelp-specific customizations"
  :lighter yelp-mode-text)

(defvar yelp-mode-text 
  (format " [yelp%s]" (if (current-branchname)
						  (concat " " (current-branchname))
						"")))

(add-hook 'find-file-hook
		  (lambda ()
			(when (current-branch)
			  (yelp-set-modeline))))

(defun yelp-set-modeline ()
  (setq yelp-mode-text (format " [yelp %s]" (current-branchname))))

(defvar yelp-mode-keymap nil
  "Keymap for yelp minor mode.")

(if yelp-mode-keymap
    nil
  (progn
    (setq yelp-mode-keymap (make-sparse-keymap))
    (define-key yelp-mode-keymap (kbd "C-c t") 'toggle-test-file)
    (define-key yelp-mode-keymap (kbd "C-c m") 'make-branch)
	(define-key yelp-mode-keymap (kbd "C-c f") 'open-from-pg)))

(provide 'yelp)