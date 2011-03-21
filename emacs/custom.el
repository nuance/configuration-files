(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 203 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(cursor-type (quote box) t)
 '(default-frame-alist (quote ((vertical-scroll-bars) (menu-bar-lines . 0) (fringe) (right-fringe) (left-fringe . 1) (internal-border-width . 0) (cursor-type . box) (background-color . "#141414") (background-mode . dark) (border-color . "black") (cursor-color . "#A7A7A7") (foreground-color . "#F8F8F8") (mouse-color . "sienna1") (tool-bar-lines . 1))))
 '(diff-switches "-u")
 '(display-time-day-and-date t)
 '(erc-track-exclude-server-buffer t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-showcount t)
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Utilities/Emacs.app/Contents/MacOS/bin" "/usr/local/bin" "/usr/local/git/bin")))
 '(gnus-cache-enter-articles (quote (ticked dormant unread read)))
 '(gnus-cache-remove-articles nil)
 '(gnus-cacheable-groups nil)
 '(gnus-fetch-old-headers t)
 '(gnus-use-cache t)
 '(mmm-submode-decoration-level 2)
 '(org-agenda-custom-commands (quote (("d" todo "DELEGATED" nil) ("c" todo "DONE|DEFERRED|CANCELLED" nil) ("w" todo "WAITING" nil) ("W" agenda "" ((org-agenda-ndays 21))) ("A" agenda "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) (org-agenda-ndays 1) (org-agenda-overriding-header "Today's Priority #A tasks: "))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote regexp) "<[^>
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (file-expand-wildcards "~/Dropbox/org/*.org"))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Dropbox/org/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote ((116 "* TODO %?
  %u" "~/Dropbox/org/work.org" "Tasks") (110 "* %u %?" "~/Dropbox/org/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(pastebin-default-domain "pb.yelpcorp.com")
 '(pastebin-domain-versions (quote (("pastebin.com" "/api_public.php") ("pastebin.example.com" "/pastebin.php") ("pb.yelpcorp.com" "/pastebin.php"))))
 '(py-indent-offset 4)
 '(py-smart-indentation nil)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(simplenote-notes-mode (quote markdown-mode))
 '(speedbar-default-position (quote left))
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(set-default-font "-apple-Inconsolata-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#141414" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "apple" :family "Consolas"))))
 '(highline-face ((t (:background "gray15"))))
 '(mode-line ((t (:background "grey20" :foreground "grey85")))))
