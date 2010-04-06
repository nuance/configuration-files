;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

(defun esk-pp-json ()
  "Pretty-print the json object following point."
  (interactive)
  (require 'json)
  (let ((json-object (save-excursion (json-read))))
    (switch-to-buffer "*json*")
    (delete-region (point-min) (point-max))
    (insert (pp json-object))
    (goto-char (point-min))))

(require 'espresso)
(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))

(provide 'nuance-js)
;;; starter-kit-js.el ends here
