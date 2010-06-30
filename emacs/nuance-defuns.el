;;-----------------------------------------------------------------------------
;; Smarter Tab Behavior
;;-----------------------------------------------------------------------------

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun my-tab-fix ()
  (local-set-key "\t" 'indent-or-expand))
  
(defun my-java-tab-fix ()
  (local-set-key "\t" 'indent-tab-or-expand))

(defun indent-tab-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (tab-to-tab-stop)))
    
(defun hl-warn-keywords ()
  (font-lock-add-keywords nil
                            '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))


;;-----------------------------------------------------------------------------
;; smarter go-to-line
;;-----------------------------------------------------------------------------

(defun smarter-goto-line (line)
  "Like goto-line, but knows about code folding"
  (interactive)
  (goto-line line)
  (show-subtree)
  (goto-line line))

;;-----------------------------------------------------------------------------
;; programming
;;-----------------------------------------------------------------------------

(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

;; Completion for all commands (eg M-x completion)

(setq ido-execute-command-cache nil)
(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
	(ido-completing-read
	 "M-x "
	 (progn
	   (unless ido-execute-command-cache
		 (mapatoms (lambda (s)
					 (when (commandp s)
					   (setq ido-execute-command-cache
							 (cons (format "%S" s) ido-execute-command-cache))))))
	   ido-execute-command-cache)))))

(defun ido-reset-command-cache ()
  (interactive)
  (setq ido-execute-command-cache nil))

(add-hook 'ido-setup-hook
		  (lambda ()
			(setq ido-enable-flex-matching t)
			(global-set-key "\M-x" 'ido-execute-command)))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
	(let ((enable-recursive-minibuffers t))
	  (visit-tags-table-buffer))
	(ido-completing-read "Project file: "
						 (tags-table-files)
						 nil t)))


(defun wicked/remember-review-file ()
  "Open `remember-data-file'."
  (interactive)
  (find-file-other-window remember-data-file))

(defun wicked/remember-line-numbers-and-file-names ()
  "Return FILENAME line NUMBER."
  (save-restriction
	(widen)
	(format " %s line %d"
			(or (buffer-file-name) (buffer-name))
			(line-number-at-pos))))

(defun paper-get-buffer ()
  (let (time-now buffer)
	(setq time-now (current-time))
	(get-buffer-create (format "*%s<%s-%s-%s>*"
							   "papers"
							   (nth 0 time-now) (nth 1 time-now) (nth 2 time-now)))))

(defun download-paper (url)
  "Download a paper url in the background when referenced from an
                                           org-mode link, returning the new (local) url"
  (interactive)
  ;; Check for the directory
  (unless (file-exists-p org-paper-directory)
	(make-directory org-paper-directory)
	(message "Created directory %s for downloaded papers." org-paper-directory))
  ;; Download.
  (let* ((url-request-method "GET")
		 (url-request-extra-headers nil)
		 (url-mime-accept-string "*/*")
		 (parsed-url (url-generic-parse-url url))
		 (download-buffer (paper-get-buffer))
		 (download-buffer-name (buffer-name download-buffer))
		 (paper-url url)
		 (file-path (concat org-paper-directory (file-name-nondirectory paper-url))))
	(with-current-buffer (get-buffer download-buffer-name)
	  ;; Bind download buffer with local buffer.
	  (setq download-buffer
			(url-retrieve-synchronously parsed-url))
	  ;; Now save the buffer in the papers directory
	  ;; Save file.
	  (with-current-buffer download-buffer
		(if (or (not (file-exists-p file-path))
				(yes-or-no-p (format "Do you want replace file: '%s' ?" file-path)))
			(progn
			  (write-file file-path)))))
	;; return the file name
	(concat "file:" (file-truename file-path))))

(defun downloadable-pdf (url)
  (if (and url
		   (or (string-equal (file-name-extension url) "pdf")
			   (string-equal (file-name-extension url) "ps")
			   (string-equal (file-name-extension url) "eps"))
		   (or (string-equal (car (split-string url ":")) "http")
			   (string-equal (car (split-string url ":")) "https")))
	  t
	nil))

(defun my-org-insert-link (&optional COMPLETE-FILE LINK-LOCATION)
  ;;(before org-insert-link (&optional COMPLETE-FILE LINK-LOCATION))
  "Rewrite remote pdf references to local files, prompting for the
                                         link location as usual then calling the insert link function with a
                                         link to the fetched pdf"
  (interactive)
  (let ((url (if LINK-LOCATION LINK-LOCATION (read-from-minibuffer
											  "Link: "))))
	(if (downloadable-pdf url)
		(org-insert-link COMPLETE-FILE (download-paper url))
	  (org-insert-link COMPLETE-FILE url))))



(defun load-dark-settings ()
  (interactive)
  (color-theme-twilight)
  ;;  (highline-mode)
  )

(defun load-light-settings ()
  (interactive)
  (color-theme-calm-forest)
  ;;  (highline-mode)
  )


;;-----------------------------------------------------------------------------
;; Smarter Centering
;;-----------------------------------------------------------------------------

;; Centering code stolen from somewhere and restolen from
;; http://www.chrislott.org/geek/emacs/dotemacs.html
;; centers the screen around a line...

(defun centerer ()
  "Repositions current line: once middle, twice top, thrice bottom"
  (interactive)
  (cond ((eq last-command 'centerer2)   ; 3 times pressed = bottom
		 (recenter -1))
		((eq last-command 'centerer1)   ; 2 times pressed = top
		 (recenter 0)
		 (setq this-command 'centerer2))
		(t                              ; 1 time pressed = middle
		 (recenter)
		 (setq this-command 'centerer1))))


 ;;-----------------------------------------------------------------------------
 ;; Indent whole buffer
 ;;-----------------------------------------------------------------------------

 (defun indent-whole-buffer ()
   "indent whole buffer"
   (interactive)
   (delete-trailing-whitespace)
   (indent-region (point-min) (point-max) nil)
   (untabify (point-min) (point-max)))
   
   

   (defun regen-autoloads (&optional force-regen)
     "Regenerate the autoload definitions file if necessary and load it."
     (interactive "P")
     (let ((autoload-dir (concat dotfiles-dir "/elpa-to-submit"))
           (generated-autoload-file autoload-file))
       (when (or force-regen
                 (not (file-exists-p autoload-file))
                 (some (lambda (f) (file-newer-than-file-p f autoload-file))
                       (directory-files autoload-dir t "\\.el$")))
         (message "Updating autoloads...")
         (update-directory-autoloads autoload-dir)))
     (load autoload-file))

(provide 'nuance-defuns)
