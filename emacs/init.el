;;; init.el --- Where all the magic begins
;;
;; Based on init.el of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
;; (add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/jabber"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/color-theme"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/mmm-mode"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/notmuch"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/malabar-1.4.0/lisp"))

;; All my programming mode customizations
(add-to-list 'load-path (concat dotfiles-dir "/programming"))

;; system
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

;;(require 'byte-code-cache)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)

;; private config variables, not stored in git
(require 'yelp-private)

;; Load up ELPA, the package manager

(require 'package)
(package-initialize)
;;(require 'nuance-elpa)

;; Load up nuance customizations

(require 'nuance-defuns)
(require 'nuance-bindings)
(require 'nuance-misc)
(require 'nuance-minor)
(require 'nuance-eshell)
(require 'nuance-programming)

(require 'nuance-colortheme)

(require 'nuance-org)

(regen-autoloads)
(load custom-file 'noerror)

;; Work around a bug on OS X where system-name is FQDN
(setq system-name (car (split-string system-name "\\.")))

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))
  
(server-start)

;;; init.el ends here