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

(require 'egg)
;; Make egg usable
(setq egg-auto-update nil)

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

(global-set-key (kbd "C-c R") 'wicked/remember-review-file)

;;-----------------------------------------------------------------------------
;; Org mode
;;-----------------------------------------------------------------------------

;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; (define-key mode-specific-map [?a] 'org-agenda)

;; (add-hook 'org-mode-hook
;;           '(lambda ()
;;              (define-key org-mode-map (kbd "C-c C-l") 'my-org-insert-link)))

;; (setq org-paper-directory "~/org-papers/")

;; (setq org-export-with-LaTeX-fragments t)

;; (setq org-todo-keywords
;; 	  '((sequence "EXPLORE(e!)" "DESIGN(s!)" "CODE(c!)" "TEST(t!)" "REVIEW(r!)" "PUSH(p!)" "|" "DONE(d!)")))

;; Set to the location of your Org files on your local system
;;(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
;;(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;;(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; (require 'org-babel-init)
;; (require 'org-babel-python)
;; (require 'org-babel-R)
;; (require 'org-babel-ditaa)
;; (require 'org-babel-sql)
;; (require 'org-babel-javascript)
;; (org-babel-load-library-of-babel)

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

(require 'nnir)

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

(provide 'nuance-misc)
