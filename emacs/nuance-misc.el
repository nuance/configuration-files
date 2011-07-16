;;-----------------------------------------------------------------------------
;; Global Defaults
;;-----------------------------------------------------------------------------

;; bell gets really annoying
(setq visible-bell t)
(setq ring-bell-function (lambda ()))   ;get rid of bell completely

;; Some better defaults
(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(setq inhibit-startup-message t)        ;no splash screen
(setq ls-lisp-dirs-first t)             ;display dirs first in dired
(setq x-select-enable-clipboard t)      ;use system clipboard
(show-paren-mode 1)                     ;match parenthesis
(menu-bar-mode -1)                      ;hide menu-bar
(scroll-bar-mode -1)                    ;hide scroll-bar
(tool-bar-mode -1)                      ;hide tool-bar
(column-number-mode 1)                  ;show column number
(global-font-lock-mode 1)               ;Color syntax highlighting
(icomplete-mode 1)
(auto-compression-mode 1) ; Use compressed files as if they were normal
(setq font-lock-maximum-decoration t)      ;why not?
(add-hook 'text-mode-hook 'auto-fill-mode) ;auto-fill
(setq transient-mark-mode t)               ;highlights selections
(setq comment-style 'plain)
(setq frame-title-format (list '("emacs ") '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(setenv "PATH" (concat "/usr/local/bin:/usr/local/git/bin:" (getenv "PATH")))

;;(require 'egg)
;; Make egg usable
;;(setq egg-auto-update nil)

(require 'printing)
(require 'cool-stuff)

;;-----------------------------------------------------------------------------
;; Tramp mode
;;-----------------------------------------------------------------------------

(require 'tramp)
(setq tramp-default-method "sshx")
(setq tramp-default-user "mattj")
(setq tramp-default-host "dev01")

;;-----------------------------------------------------------------------------
;; ido - Better minibuffer completion
;;-----------------------------------------------------------------------------
(if (< emacs-major-version 22) ;newer version included in Emacs 22, that doesn't work with 21
    (load-library "ido-old")
  (require 'ido))
(ido-mode t)

(add-hook 'ido-define-mode-map-hook 'ido-my-keys)
(defun ido-my-keys ()
  (define-key 'ido-mode-map "\t" 'ido-complete)
  (define-key 'ido-mode-map "\C-t" 'ido-toggle-regexp) ; same as in isearch
  (define-key 'ido-mode-map "\C-d" 'ido-enter-dired))  ; cool


;;(global-set-key "\C-x\C-f" 'ido-find-file-in-tag-files)

;;-----------------------------------------------------------------------------
;; Remember mode
;;-----------------------------------------------------------------------------

(require 'remember)

(setq remember-data-file "~/notes.txt")
(global-set-key (kbd "C-c r") 'remember)

(eval-after-load 'remember
  '(progn
     (add-to-list 'remember-annotation-functions
                  'wicked/remember-line-numbers-and-file-names)))

;; (global-set-key (kbd "C-c R") 'wicked/remember-review-file)

;;-----------------------------------------------------------------------------
;; Org mode
;;-----------------------------------------------------------------------------

(defun export-for-candidate ()
  (interactive)
  (let ((old-tags org-export-select-tags)
		(old-excludes org-export-exclude-tags)
		(candidate-type (ido-completing-read "Tag: " '("webdev" "search" "backend"))))
	(setq org-export-select-tags '("candidate"))
	(setq org-export-exclude-tags (remove candidate-type '("webdev" "search" "backend")))
	(org-export)
	(setq org-export-select-tags old-tags)
	(setq org-export-exclude-tags old-excludes)))

(defun export-for-interview ()
  (interactive)
  (let ((old-tags org-export-select-tags)
		(old-excludes org-export-exclude-tags)
		(candidate-type (ido-completing-read "Tag: " '("webdev" "search" "backend"))))
	(setq org-export-exclude-tags (remove candidate-type '("webdev" "search" "backend")))
	(org-export)
	(setq org-export-select-tags old-tags)
	(setq org-export-exclude-tags old-excludes)))

;;-----------------------------------------------------------------------------
;; Gnus gmail
;;-----------------------------------------------------------------------------

;; (require 'nnir)

;;-----------------------------------------------------------------------------
;; PO Mode
;;-----------------------------------------------------------------------------

(require 'po-mode)

;;-----------------------------------------------------------------------------
;; simplenote
;;-----------------------------------------------------------------------------

(require 'simplenote)

;;-----------------------------------------------------------------------------
;; markdown
;;-----------------------------------------------------------------------------

(require 'markdown-mode)

;;-----------------------------------------------------------------------------
;; edit from chrome
;;-----------------------------------------------------------------------------

(require 'edit-server)
(setq edit-server-new-frame nil)
(add-hook 'edit-server-done-hook 'on-edit-server-done-do-backup)
(edit-server-start)

(defun on-edit-server-done-do-backup ()
  "Run when text is sent to Google Chrome. Do a backup of the
    stuff sent there in case something goes wrong, e.g. Chrome
    crashes."
  (let* ((backup-dir "~/._emacs_chrome-backup")
		 (backup-file (format "%s.txt" (float-time)))
		 (backup-path (concat backup-dir "/" backup-file)))
	(unless (file-directory-p backup-dir)
	  (make-directory backup-dir))
	(write-region (point-min) (point-max) backup-path)))

;;-----------------------------------------------------------------------------
;; dribbling (for future keyboard optimization)
;;-----------------------------------------------------------------------------

(open-dribble-file (format-time-string "~/.dribbles/%m%d-%R.dribble" (current-time)))

;;-----------------------------------------------------------------------------
;; auto-hide
;;-----------------------------------------------------------------------------

;; (autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
;; (global-set-key "\C-ch" 'hide-lines)

;; (defadvice show-hidden-lines
;;   (after show-all activate)
;;   """ Show invisible lines """
;;   (message "calling show invisible")
;;   (show-all-invisible))

;;(require 'notmuch)
;;(setq notmuch-command "/Users/matt/bin/remote-notmuch")

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(require 'simplenote)
(simplenote-setup)

;; (require 'gtk-doc)

(provide 'nuance-misc)
