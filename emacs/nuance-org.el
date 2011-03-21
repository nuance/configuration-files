(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key mode-specific-map [?a] 'org-agenda)

(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))))

(add-hook 'remember-mode-hook 'org-remember-apply-template)

(define-key global-map [(control meta ?r)] 'remember)

(setq org-directory "~/Dropbox/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(custom-set-variables
 '(org-agenda-files (file-expand-wildcards "~/Dropbox/org/*.org"))
 '(org-default-notes-file "~/Dropbox/org/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
       ("c" todo "DONE|DEFERRED|CANCELLED" nil)
       ("w" todo "WAITING" nil)
       ("W" agenda "" ((org-agenda-ndays 21)))
       ("A" agenda ""
        ((org-agenda-skip-function
          (lambda nil
        (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
         (org-agenda-ndays 1)
         (org-agenda-overriding-header "Today's Priority #A tasks: ")))
       ("u" alltodo ""
        ((org-agenda-skip-function
          (lambda nil
        (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                      (quote regexp) "<[^>\n]+>")))
         (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote ((116 "* TODO %?\n  %u" "~/Dropbox/org/work.2011.q1.org" "Tasks")
		   (109 "* %?\n  %u" "~/Dropbox/org/work.2011.q1.org" "Mobile Search Metrics")
		   (99 "* %?\n  %u" "~/Dropbox/org/work.2011.q1.org" "Contribution Search")
		   (117 "* %?\n  %u" "~/Dropbox/org/work.2011.q1.org" "University Data Set")
		   (110 "* %u %?" "~/Dropbox/org/notes.org" "Notes"))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))

(defadvice remember-other-frame (around remember-frame-parameters activate)
      "Set some frame parameters for the remember frame."
      (let ((default-frame-alist (append
    			      '(
    				(name . "*Remember*")
    				(width . 80)
    				(height . 10)
    				(vertical-scroll-bars . nil)
    				(menu-bar-lines . 0)
    				(tool-bar-lines . 0)
    				)
    			      default-frame-alist)))
        ad-do-it
        ))

(setq org-log-done 'time)

(provide 'nuance-org)
