;;; org-babel-javascript.el --- org-babel functions for javascript evaluation

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating javascript source code.

;;; Code:
(require 'org-babel)
(require 'js2)

(org-babel-add-interpreter "js")

(add-to-list 'org-babel-tangle-langs '("javascript" "js" "#!/usr/bin/env js"))

(defun org-babel-execute:javascript (body params)
  "Execute a block of Javascript code with org-babel.  This function is
called by `org-babel-execute-src-block' via multiple-value-bind."
  (message "executing Javascript source code block")
  (let ((full-body (concat
		    (mapconcat ;; define any variables
		     (lambda (pair)
		       (format "%s=%s"
			       (car pair)
			       (org-babel-javascript-var-to-javascript (cdr pair))))
		     vars "\n") "\n" (org-babel-trim body) "\n")) ;; then the source block body
	(session (org-babel-javascript-initiate-session session)))
    (org-babel-javascript-evaluate session full-body result-type)))

(defun org-babel-prep-session:javascript (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-javascript-initiate-session session))
         (vars (org-babel-ref-variables params))
         (var-lines (mapcar ;; define any variables
                     (lambda (pair)
                       (format "%s=%s"
                               (car pair)
                               (org-babel-javascript-var-to-javascript (cdr pair))))
                     vars)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (move-end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))))

;; helper functions

(defun org-babel-javascript-var-to-javascript (var)
  "Convert an elisp var into a string of javascript source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-javascript-var-to-javascript var ", ") "]")
    (format "%S" var)))

(defun org-babel-javascript-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (string-match "^\\[.+\\]$" results)
       (org-babel-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               ", " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     results)))

(defvar org-babel-javascript-buffers '(:default . nil))

(defun org-babel-javascript-session-buffer (session)
  (cdr (assoc session org-babel-javascript-buffers)))

(defun org-babel-javascript-initiate-session-by-key (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (save-window-excursion
    (let* ((session (if session (intern session) :default))
           (javascript-buffer (org-babel-javascript-session-buffer session)))
      (run-javascript)
      (setq org-babel-javascript-buffers (cons (cons session javascript-buffer)
					   (assq-delete-all session org-babel-javascript-buffers)))
      session)))

(defun org-babel-javascript-initiate-session (&optional session)
  (unless (string= session "none")
    (org-babel-javascript-session-buffer (org-babel-javascript-initiate-session-by-key session))))

(defvar org-babel-javascript-last-value-eval "_"
  "When evaluated by Javascript this returns the return value of the last statement.")
(defvar org-babel-javascript-eoe-indicator "'org_babel_javascript_eoe'"
  "Used to indicate that evaluation is has completed.")
(defvar org-babel-javascript-wrapper-method
  "
def main():
%s

open('%s', 'w').write( str(main()) )")

(defun org-babel-javascript-evaluate (buffer body &optional result-type)
  "Pass BODY to the Javascript process in BUFFER.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (if (not session)
      ;; external process evaluation
      (save-window-excursion
        (case result-type
          (output
           (with-temp-buffer
             (insert body)
             ;; (message "buffer=%s" (buffer-string)) ;; debugging
             (shell-command-on-region (point-min) (point-max) "javascript" 'replace)
             (buffer-string)))
          (value
           (let ((tmp-file (make-temp-file "javascript-functional-results")))
             (with-temp-buffer
               (insert
		(format
		 org-babel-javascript-wrapper-method
		 (let ((lines (split-string
			       (org-remove-indentation (org-babel-trim body)) "[\r\n]")))
		   (concat
		    (mapconcat
		     (lambda (line) (format "\t%s" line))
		     (butlast lines) "\n")
		    (format "\n\treturn %s" (last lines))))
		 tmp-file))
               ;; (message "buffer=%s" (buffer-string)) ;; debugging
               (shell-command-on-region (point-min) (point-max) "javascript"))
             (org-babel-javascript-table-or-string
	      (with-temp-buffer (insert-file-contents tmp-file) (buffer-string)))))))
    ;; comint session evaluation
    (org-babel-comint-in-buffer buffer
      (let* ((raw (org-babel-comint-with-output buffer org-babel-javascript-eoe-indicator t
                    ;; for some reason javascript is fussy, and likes enters after every input
                    (mapc (lambda (statement) (insert statement) (comint-send-input nil t))
                          (split-string (org-babel-trim full-body) "[\r\n]+"))
                    (comint-send-input nil t) (comint-send-input nil t)
                    (insert org-babel-javascript-last-value-eval)
                    (comint-send-input nil t)
                    (insert org-babel-javascript-eoe-indicator)
                    (comint-send-input nil t)))
             (results (delete org-babel-javascript-eoe-indicator
                              (cdr (member org-babel-javascript-eoe-indicator
                                           (reverse (mapcar #'org-babel-trim raw)))))))
        (setq results (mapcar #'org-babel-javascript-read-string results))
        (case result-type
	  (output (org-babel-trim (mapconcat #'identity (reverse (cdr results)) "\n")))
	  (value (org-babel-javascript-table-or-string (org-babel-trim (car results)))))))))

(defun org-babel-javascript-read-string (string)
  "Strip 's from around ruby string"
  (if (string-match "'\\([^\000]+\\)'" string)
      (match-string 1 string)
    string))

(provide 'org-babel-javascript)
;;; org-babel-javascript.el ends here
