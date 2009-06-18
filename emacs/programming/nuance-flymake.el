-------------------------------------------------------------
;; Flymake - error checking as you type
;;-----------------------------------------------------------------------------

;; pyflakes... thanks again to chrism.

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(load-library "flymake-cursor")

(defun pylint ()
  "Run pylint against the file behind the current buffer after
checking if unsaved buffers should be saved."

  (interactive)
  (let* ((file (buffer-file-name (current-buffer)))
         (command (concat "pylint --output-format=parseable \"" file "\"")))
    (save-some-buffers (not compilation-ask-about-save) nil) ; save files.
    (compilation-start command)))

;;(when (load "flymake" t)
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))


(defun strict-flymake-enable ()
  (interactive)
  (delete 'flymake-allowed-file-name-masks
          '("\\.py\\'" flymake-pyflakes-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init))
  (flymake-start-syntax-check))

(defun strict-flymake-disable ()
  (interactive)
  (delete 'flymake-allowed-file-name-masks
          '("\\.py\\'" flymake-pylint-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
  (flymake-start-syntax-check))

(defun next-flymake-error ()
  (interactive)
  (let ((err-buf nil))
    (condition-case err
        (setq err-buf (next-error-find-buffer))
      (error))
    (if err-buf
        (next-error)
      (progn
        (flymake-goto-next-error)
        (let ((err (get-char-property (point) 'help-echo)))
          (when err
            (message err)))))))

;;(add-hook 'find-file-hook 'flymake-find-file-hook)


(provide 'nuance-flymake)