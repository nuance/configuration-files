;;-----------------------------------------------------------------------------
;; Yelp services
;;-----------------------------------------------------------------------------

(require 'yelp-private)

;; IRC

(defun irc ()
  (interactive)
  (require 'tls)
  (require 'erc)
  (setq erc-auto-query 'buffer)
  (setq erc-track-exclude-types '("NICK"))
  (setq erc-track-use-faces t)
  (setq erc-track-faces-priority-list '(erc-current-nick-face))

  (setq erc-autojoin-channels-alist
		'(("irc.yelpcorp.com" "#yelp")))
  (erc-tls :server *yelp-irc-host* 
		   :port *yelp-irc-port* 
		   :nick *yelp-username* 
		   :password *yelp-irc-password*))

;; Mail

(setq user-mail-address (concat *yelp-username* "@yelp.com"))
(setq user-full-name *yelp-fullname*)

(setq gnus-select-method '(nnimap "gmail"
								  (nnimap-address "imap.gmail.com")
								  (nnimap-server-port 993)
								  (nnimap-stream ssl)
								  (gnus-summary-line-format "%U%R%i %B%(%[%4L: %-23,23f%]%) %s\n")
								  (nnir-search-engine imap)))

(setq message-send-mail-function 'smtpmail-send-it
	  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	  smtpmail-auth-credentials '(("smtp.gmail.com" 587 user-mail-address nil))
	  smtpmail-default-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 587)

;; Chat

(add-to-list jabber-account-list
			 '("mattj@yelp.com"
			   (:network-server . "talk.google.com")
			   (:port . 443)
			   (:connection-type . ssl)))

;;-----------------------------------------------------------------------------
;; Yelp helper code
;;-----------------------------------------------------------------------------

(defun yelp-local-name (filename)
   ;; Found / on a tramp connection
   (if (tramp-tramp-file-p filename) 
	   (tramp-file-name-localname (tramp-dissect-file-name filename))
	 filename))

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
   ((string-equal (yelp-local-name parent) "/") nil)
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
  (ido-find-file-in-dir (current-branch)))

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

(defun yelp-branch-is-loc-p ()
  "Return t if loc points to the current branch"
  (let ((branch (current-branch "pg")))
	(if (not branch) nil
	  (let ((loc-dir (concat (file-name-directory (directory-file-name branch)) "loc")))
		(string-equal (yelp-local-name (file-symlink-p loc-dir)) (yelp-local-name branch))))))

;; Switch loc
(defun switch-loc ()
  "Change the loc pointer to point to the branch associated with the current buffer"
  (interactive)
  (let* ((branch (current-branch "pg"))
		 (pg (file-name-directory (directory-file-name branch))))
	(shell-command (format "cd %s && rm loc" (yelp-local-name pg)))
	(shell-command (format "ln -s %s %sloc" (yelp-local-name branch) (yelp-local-name pg)))
	(yelp-update-buffers)))

;; Make templates
(defun make-branch (&optional branch)
  (interactive)
  (let ((branch (if branch branch (current-branch "pg"))))
	(cd branch)
	(compile "make")))

(defun yelp-up ()
  (interactive)
  (switch-loc)
  (shell-command (format "cd %s && make -j8 && myapachectl -k graceful" 
						 (yelp-local-name (current-branch)))))

;;=====================================
;; Misc
;;=====================================

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
  (compile "myapachectl -k graceful"))

;; Start/Stop lucene


;;(setq load-path (append load-path  "/usr/share/emacs/site-lisp/w3m"))
;;(require 'w3m)

;; Screenshot
(defun screenshot ()
  (interactive)
  (save-excursion
	(pop-to-buffer "*gecko*")
	(delete-region (point-min) (point-max))
	(insert-image (w3m-create-image "http://localhost:4242/screenshot/http://www.mattj.dev.yelp.com/user_details_quicktips?userid=qjYYheQzd_K7_UjiRBvEpA"))))

;;=====================================
;; Mode definition
;;=====================================

(defun yelp-update-buffers (&optional start buffers)
  (interactive)
  (let ((rem (if (and (null buffers) (null start)) (buffer-list) buffers)))
	(unless (null rem)
	  (yelp-update-buffer (car rem))
	  (yelp-update-buffers t (cdr rem)))))

(defun yelp-update-buffer (buffer)
  (with-current-buffer buffer
	(yelp-set-mode-text)
	(force-mode-line-update)))

;;(make-variable-buffer-local 'yelp-mode)
(make-variable-buffer-local 'yelp-mode-text)

(defvar yelp-mode-keymap nil
  "Keymap for yelp minor mode.")

(if yelp-mode-keymap
    nil
  (progn
    (setq yelp-mode-keymap (make-sparse-keymap))
    (define-key yelp-mode-keymap (kbd "C-c s") 'switch-loc)
    (define-key yelp-mode-keymap (kbd "C-c r") 'run-test)
    (define-key yelp-mode-keymap (kbd "C-c t") 'toggle-test-file)
    (define-key yelp-mode-keymap (kbd "C-c m") 'make-branch)
    (define-key yelp-mode-keymap (kbd "C-c u") 'yelp-up)
	(define-key yelp-mode-keymap (kbd "C-c f") 'open-from-pg)))

(defun yelp-modeline () 
  (format " [yelp%s%s]" (if (current-branchname)
							(concat " " (current-branchname))	"")
		  (if (yelp-branch-is-loc-p) "*" "")))

(defvar yelp-mode-text (yelp-modeline))

(defun yelp-set-mode-text ()
  (interactive)
  "Set's the modeline to [yelp (branch for current file)(* if the branch is loc)]"
  (setq yelp-mode-text (yelp-modeline)))

(define-minor-mode yelp-mode
  "Yelp-specific customizations"
  :global t
  :lighter yelp-mode-text
  :keymap yelp-mode-keymap
  (yelp-set-mode-text))

(add-hook 'find-file-hook
		  (lambda ()
			(when (and yelp-mode (current-branch))
			  (yelp-set-mode-text))))

(provide 'yelp)
