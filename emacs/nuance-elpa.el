;;; nuance-elpa.el --- Install a base set of packages automatically.
;;
;; Derived from the Emacs Starter Kit

(defvar starter-kit-packages (list 'highline
        'javascript
                                'js2-mode
                                'lua-mode
                                   'css-mode
                                   'nxml)
  "Libraries that should be installed by default.")

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.
(when (esk-online?) (ignore-errors (with-timeout (15)
                                     (unless package-archive-contents (package-refresh-contents))
                                     (starter-kit-elpa-install))))

(provide 'nuance-elpa)