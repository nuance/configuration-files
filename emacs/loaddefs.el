;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (highline-split-window-horizontally highline-split-window-vertically
;;;;;;  highline-view-mode highline-mode global-highline-mode highline-customize)
;;;;;;  "highline" "elpa-to-submit/highline.el" (18965 51027))
;;; Generated autoloads from elpa-to-submit/highline.el

(autoload 'highline-customize "highline" "\
Customize highline group.

\(fn)" t nil)

(defvar global-highline-mode nil "\
Non-nil if Global-Highline mode is enabled.
See the command `global-highline-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-highline-mode'.")

(custom-autoload 'global-highline-mode "highline" nil)

(autoload 'global-highline-mode "highline" "\
Toggle global minor mode to highlight line about point (HL on modeline).

If ARG is null, toggle global highline mode.
If ARG is a number and is greater than zero, turn on
global highline mode; otherwise, turn off global highline mode.
Only useful with a windowing system.

\(fn &optional ARG)" t nil)

(autoload 'highline-mode "highline" "\
Toggle local minor mode to highlight the line about point (hl on modeline).

If ARG is null, toggle local highline mode.
If ARG is a number and is greater than zero, turn on
local highline mode; otherwise, turn off local highline mode.
Only useful with a windowing system.

\(fn &optional ARG)" t nil)

(autoload 'highline-view-mode "highline" "\
Toggle indirect mode to highlight current line in buffer (Ihl on modeline).

If ARG is null, toggle indirect highline mode.
If ARG is a number and is greater than zero, turn on
indirect highline mode; otherwise, turn off indirect highline mode.
Only useful with a windowing system.

Indirect highline (`highline-view-mode') is useful when you wish
to have various \"visions\" of the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagically killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See `highline-view-prefix'.

\(fn &optional ARG)" t nil)

(autoload 'highline-split-window-vertically "highline" "\
Split window vertically then turn on indirect highline mode.

See `split-window-vertically' for explanation about ARG and for
documentation.

See also `highline-view-mode' for documentation.

\(fn &optional ARG)" t nil)

(autoload 'highline-split-window-horizontally "highline" "\
Split window horizontally then turn on indirect highline mode.

See `split-window-horizontally' for explanation about ARG and for
documentation.

See also `highline-view-mode' for documentation.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (javascript-mode) "javascript" "elpa-to-submit/javascript.el"
;;;;;;  (18965 51027))
;;; Generated autoloads from elpa-to-submit/javascript.el

(autoload 'javascript-mode "javascript" "\
Major mode for editing JavaScript source text.

Key bindings:

\\{javascript-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (js2-mode) "js2" "elpa-to-submit/js2.el" (18965
;;;;;;  51027))
;;; Generated autoloads from elpa-to-submit/js2.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (global-linum-mode linum-mode linum-format) "linum"
;;;;;;  "elpa-to-submit/linum.el" (18965 51027))
;;; Generated autoloads from elpa-to-submit/linum.el

(defvar linum-format 'dynamic "\
Format used to display line numbers. Either a format string
like \"%7d\", 'dynamic to adapt the width as needed, or a
function that is called with a line number as its argument and
should evaluate to a string to be shown on that line. See also
`linum-before-numbering-hook'.")

(custom-autoload 'linum-format "linum" t)

(autoload 'linum-mode "linum" "\
Toggle display of line numbers in the left marginal area.

\(fn &optional ARG)" t nil)

(defvar global-linum-mode nil "\
Non-nil if Global-Linum mode is enabled.
See the command `global-linum-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-linum-mode'.")

(custom-autoload 'global-linum-mode "linum" nil)

(autoload 'global-linum-mode "linum" "\
Toggle Linum mode in every possible buffer.
With prefix ARG, turn Global-Linum mode on if and only if ARG is positive.
Linum mode is enabled in all buffers where `linum-on' would do it.
See `linum-mode' for more information on Linum mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (lua-mode) "lua-mode" "elpa-to-submit/lua-mode.el"
;;;;;;  (18965 51027))
;;; Generated autoloads from elpa-to-submit/lua-mode.el

(autoload 'lua-mode "lua-mode" "\
Major mode for editing Lua code.
The following keys are bound:
\\{lua-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;;***

;;;### (autoloads (jython-mode python-mode run-python) "python" "elpa-to-submit/python.el"
;;;;;;  (18965 51027))
;;; Generated autoloads from elpa-to-submit/python.el

(add-to-list 'interpreter-mode-alist '("jython" . jython-mode))

(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(autoload 'run-python "python" "\
Run an inferior Python process, input and output via buffer *Python*.
CMD is the Python command to run.  NOSHOW non-nil means don't show the
buffer automatically.

Normally, if there is a process already running in `python-buffer',
switch to that buffer.  Interactively, a prefix arg allows you to edit
the initial command line (default is `python-command'); `-i' etc. args
will be added to this as appropriate.  A new process is started if:
one isn't running attached to `python-buffer', or interactively the
default `python-command', or argument NEW is non-nil.  See also the
documentation for `python-buffer'.

Runs the hook `inferior-python-mode-hook' (after the
`comint-mode-hook' is run).  (Type \\[describe-mode] in the process
buffer for a list of commands.)

\(fn &optional CMD NOSHOW NEW)" t nil)

(autoload 'python-mode "python" "\
Major mode for editing Python files.
Turns on Font Lock mode unconditionally since it is currently required
for correct parsing of the source.
See also `jython-mode', which is actually invoked if the buffer appears to
contain Jython code.  See also `run-python' and associated Python mode
commands for running Python under Emacs.

The Emacs commands which work with `defun's, e.g. \\[beginning-of-defun], deal
with nested `def' and `class' blocks.  They take the innermost one as
current without distinguishing method and class definitions.  Used multiple
times, they move over others at the same indentation level until they reach
the end of definitions at that level, when they move up a level.
\\<python-mode-map>
Colon is electric: it outdents the line if appropriate, e.g. for
an else statement.  \\[python-backspace] at the beginning of an indented statement
deletes a level of indentation to close the current block; otherwise it
deletes a character backward.  TAB indents the current line relative to
the preceding code.  Successive TABs, with no intervening command, cycle
through the possibilities for indentation on the basis of enclosing blocks.

\\[fill-paragraph] fills comments and multi-line strings appropriately, but has no
effect outside them.

Supports Eldoc mode (only for functions, using a Python process),
Info-Look and Imenu.  In Outline minor mode, `class' and `def'
lines count as headers.  Symbol completion is available in the
same way as in the Python shell using the `rlcompleter' module
and this is added to the Hippie Expand functions locally if
Hippie Expand mode is turned on.  Completion of symbols of the
form x.y only works if the components are literal
module/attribute names, not variables.  An abbrev table is set up
with skeleton expansions for compound statement templates.

\\{python-mode-map}

\(fn)" t nil)

(autoload 'jython-mode "python" "\
Major mode for editing Jython files.
Like `python-mode', but sets up parameters for Jython subprocesses.
Runs `jython-mode-hook' after `python-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (lisp-complete-symbol symbol-completion-try-complete
;;;;;;  symbol-complete) "sym-comp" "elpa-to-submit/sym-comp.el"
;;;;;;  (18965 51027))
;;; Generated autoloads from elpa-to-submit/sym-comp.el

(autoload 'symbol-complete "sym-comp" "\
Perform completion of the symbol preceding point.
This is done in a way appropriate to the current major mode,
perhaps by interrogating an inferior interpreter.  Compare
`complete-symbol'.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered.

This function requires `symbol-completion-completions-function'
to be set buffer-locally.  Variables `symbol-completion-symbol-function',
`symbol-completion-predicate-function' and
`symbol-completion-transform-function' are also consulted.

\(fn &optional PREDICATE)" t nil)

(autoload 'symbol-completion-try-complete "sym-comp" "\
Completion function for use with `hippie-expand'.
Uses `symbol-completion-symbol-function' and
`symbol-completion-completions-function'.  It is intended to be
used something like this in a major mode which provides symbol
completion:

  (if (featurep 'hippie-exp)
      (set (make-local-variable 'hippie-expand-try-functions-list)
	   (cons 'symbol-completion-try-complete
                 hippie-expand-try-functions-list)))

\(fn OLD)" nil nil)

(autoload 'lisp-complete-symbol "sym-comp" "\
Perform completion on Lisp symbol preceding point.
Compare that symbol against the known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered, e.g. `commandp'.
If PREDICATE is nil, the context determines which symbols are
considered.  If the symbol starts just after an open-parenthesis, only
symbols with function definitions are considered.  Otherwise, all
symbols with function definitions, values or properties are
considered.

\(fn &optional PREDICATE)" t nil)

;;;***

;;;### (autoloads (textmate-mode) "textmate" "elpa-to-submit/textmate.el"
;;;;;;  (18965 51027))
;;; Generated autoloads from elpa-to-submit/textmate.el

(defvar textmate-mode nil "\
Non-nil if Textmate mode is enabled.
See the command `textmate-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `textmate-mode'.")

(custom-autoload 'textmate-mode "textmate" nil)

(autoload 'textmate-mode "textmate" "\
TextMate Emulation Minor Mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("elpa-to-submit/byte-code-cache.el" "elpa-to-submit/cheat.el"
;;;;;;  "elpa-to-submit/color-theme-colorful-obsolescence.el" "elpa-to-submit/color-theme-twilight.el"
;;;;;;  "elpa-to-submit/cool-stuff.el" "elpa-to-submit/css-mode.el"
;;;;;;  "elpa-to-submit/flymake-cursor.el" "elpa-to-submit/mmm-mako.el"
;;;;;;  "elpa-to-submit/pair-mode.el" "elpa-to-submit/yelp.el") (18965
;;;;;;  52462 796637))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
