(defun open-diary ()
  (interactive)
  (find-file "~/.diary")
  (goto-line 2)
  (move-end-of-line nil)
  (newline)
  (time-stamp)
  (newline))

(defun save-diary ()
  (interactive)
  (cd "~/diary")
  (shell-command (format "/usr/local/bin/git commit -am\"%s\"" (format-time-string "%c" (current-time)))))

(defvar my-diary-mode-keymap nil
  "Keymap for my diary minor mode.")

(if my-diary-mode-keymap
    nil
  (progn
    (setq my-diary-mode-keymap (make-sparse-keymap))
	(define-key my-diary-mode-keymap (kbd "C-c n") 'open-diary)))

(define-minor-mode my-diary-mode
  "diary niceties"
  :global t
  :lighter " Diary"
  :keymap my-diary-mode-keymap)

(defun time-stamp ()
  "Insert a time stamp."
  (interactive)
  (insert
   (concat "* [" (format-time-string "%c" (current-time)) "] ")))
   
(provide 'nuance-diary)
