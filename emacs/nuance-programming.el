(add-hook 'coding-hook 'hl-warn-keywords)
(add-hook 'coding-hook 'my-tab-fix)

(add-hook 'c-mode-hook          'my-tab-fix)
(add-hook 'sh-mode-hook         'my-tab-fix)
(add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
(add-hook 'python-mode-hook     'my-tab-fix)
(add-hook 'java-mode-hook       'my-java-tab-fix)
(add-hook 'objc-mode-hook       'my-tab-fix)

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 0)

(require 'nuance-js)
(require 'nuance-python)

;;-----------------------------------------------------------------------------
;; ESS mode
;;-----------------------------------------------------------------------------

(require 'ess-site)

;;-----------------------------------------------------------------------------
;; Scala mode
;;-----------------------------------------------------------------------------

(require 'scala-mode-auto)

;;-----------------------------------------------------------------------------
;; Thrift mode
;;-----------------------------------------------------------------------------

;;(require 'thrift-mode)

;;-----------------------------------------------------------------------------
;; Objective-C mode
;;-----------------------------------------------------------------------------
(defun objc-outline-level ()
  0)

(add-hook 'objc-mode-hook
          (lambda ()
            (pair-mode)
            (setq outline-regexp "[ \t]*\\(@interface\\|@implementation\\)\\|[ \t]*\\(\\-\\|\\+\\)[ \t]*([^\)]*)")
            (setq outline-level 'objc-outline-level)
            (outline-minor-mode t)
            (hide-body)
            (show-paren-mode 1)))

;;-----------------------------------------------------------------------------
;; Java mode
;;-----------------------------------------------------------------------------
(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key [(return)] 'newline-and-indent)
            (custom-set-variables '(tab-width 4))))

;;-----------------------------------------------------------------------------
;; Erlang mode
;;-----------------------------------------------------------------------------

;;(when (string-equal host-name "nuance")
;;  (setq load-path (cons  "/sw/lib/erlang/lib/tools-2.6/emacs" load-path))
;;  (setq erlang-root-dir "/sw/")
;;  (setq exec-path (cons "/sw/bin" exec-path))
;;  (require 'erlang-start))

;;-----------------------------------------------------------------------------
;; Lua mode
;;-----------------------------------------------------------------------------

(require 'lua-mode)

(defun flymake-lua-init ()
  "Invoke luac with '-p' to get syntax checking"
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "luac" (list "-p" local-file))))

(when (load "flymake" t)
  (add-to-list 'flymake-allowed-file-name-masks '("\\.lua\\'" flymake-lua-init))
  (add-to-list 'flymake-err-line-patterns '("^.*luac[0-9.]*\\(.exe\\)?: *\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 2 3 nil 4)))

(add-hook 'lua-mode-hook
          '(lambda ()
             "Don't want flymake mode for lua regions in rhtml
          files and also on read only files"
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))))

;;-----------------------------------------------------------------------------
;; Haskell mode
;;-----------------------------------------------------------------------------

;; (load "~/emacs/haskell-mode/haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'font-lock-mode)

;; (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

(provide 'nuance-programming)