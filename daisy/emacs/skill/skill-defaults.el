;; Skill-mode for Skill code programming in GnuEmacs.
;; Copyright (C) 1993,1994,1995 Jonas Jarnestrom.

;; $Id: skill-defaults.el,v 1.31 1996/02/10 19:04:28 euajojm Exp $
;; Author: Jonas Jarnestrom <etxjojm@eua.ericsson.se>

;;**********************************************************
;; Administrator options, for site-local customization.
;;**********************************************************

(defvar il-end-comment-start " ; ** " 
"The start-signature of an automatically generated end-comment.
Check with your department if there exists a local preference for the
end-comment syntax, the first time you install Skill-mode, and adjust
once and for all if necessary. Avoid changing once people have started
to use Skill-mode, since all end-comments created so far will then become
write-protected.")
(put 'il-end-comment-start 'predicate 'stringp)

(defvar il-end-comment-end " **" 
"The end-signature of an automatically generated end-comment.
Not as critical as il-end-comment-start; may be changed after
the initial release to the users.")
(put 'il-end-comment-end 'predicate 'stringp)

(defvar il-complete-verbose-help t
  "Give verbose completion help in a separate help window.
When only initials are shown in the help info slot, the corresponding
full words are displayed in the help window.
Slows down performance somewhat.")
(put 'il-complete-verbose-help 'predicate 'il-t-or-nil-p)

(defvar il-header-search-limit 3
  "Number of lines to search for the file header.")
(put 'il-header-search-limit 'predicate 'natnump)

(defvar il-header-max-size 30
  "Maximum size of a file header.")
(put 'il-header-max-size 'predicate 'natnump)

(defvar il-header-user-company-name
  (or (getenv "ORGANIZATION") "Your company")
  "User's company name to be included in the copyright string.
Default value is taken from environment variable ORGANIZATION, if it exists.")
(put 'il-header-user-company-name 'predicate 'stringp)

(defvar il-header-copyright-notice nil
  "Overrides the default copyright notice in the file header.
Default is: `(C) Copyright <year>, <company-name> , all rights reserved.'
Use `\n;' to insert newlines if you need a multi-line notice.")
(put 'il-header-copyright-notice 'predicate 'il-string-or-nil-p)

(defvar il-header-VCS "CVS"
  "Name of the version control system you are using, if any.
Use uppercase letters when entering the name.
non-nil yields a VCS-line containing the VCS header keyword.
Set to nil if no version control system is used.")
(put 'il-header-VCS 'predicate 'il-string-or-nil-p)

(defvar il-header-VCS-header-keyw (concat "$" "Id" "$")
  "Header substitution keyword to put in the file header.
At check-in the VCS substitutes the keyword with header info.
The default value is appropriate for CVS and RCS.")
(put 'il-header-VCS-header-keyw 'predicate 'stringp)

(defvar il-header-VCS-history-keyw (concat "$" "Log" "$")
  "History substitution keyword to put in the revision header.
The default value is appropriate for CVS and RCS.
The benefit of this keyword is somewhat debatable. For example,
the change history may progressively grow to unwieldy proportions.
Set to nil to prohibit VCS from controling the revision header.")
(put 'il-header-VCS-history-keyw 'predicate 'il-string-or-nil-p)

(defvar il-manual-viewer-postscript "pageview -dpi 78"
  "Postscript viewer command to view the user manual.
The default setting works for SUNOS and Solaris. `ghostview' from FSF is 
another possible alternative, but doesn't seem to give as good readability
as pageview. See the man pages on `ghostview'.")
(put 'il-manual-viewer-postscript 'predicate 'stringp)

(defvar il-fundoc-begin-regexp "^;\\|/\\*"
  "Regexp for the start of doc-string of local functions.
The default pattern matches both line-oriented and block-oriented comments.
Avoid fiddling with unless you are familiar with Emacs regexps.")
(put 'il-fundoc-begin-regexp 'predicate 'stringp)

(defvar il-fundoc-end-regexp "^;.+\n[^;]\\|\\*/"
  "Regexp for the end of doc-string of local functions.
The default pattern matches both line-oriented and block-oriented comments.
Avoid fiddling with unless you are familiar with Emacs regexps.")
(put 'il-fundoc-end-regexp 'predicate 'stringp)

(defvar il-fundoc-search-limit 3
  "Number of lines from beginning-of-fun to the fun doc-string.
Defines the scope of the forward-search. Must be a positive number.")
(put 'il-fundoc-search-limit 'predicate 'natnump)

(defvar il-fun-begin-regexp "^procedure(\\|^(procedure \\|^(defun "
  "Search pattern used for determining the syntax of fun start.
The search is case-sensitive so the pattern must match exactly.
Note that this regexp is only used to find the first fun definition
in the file and store that pattern in the var il-function-start.
All subsequent funs must conform to this pattern to be recognizable.
Fun definitions must begin at left margin to be recognized.")
(put 'il-fun-begin-regexp 'predicate 'stringp)

(defvar il-fun-end-regexp "[ \t]*\\([;/].+\\)?\n *\\(\n\\|\\'\\)"
  "Syntax of end-of-function, as seen after the closing parenthesis.
The default setting means:
white space(s) or a comment followed by a blank line or end-of-buffer.
Avoid fiddling with unless you are well familiar with Emacs regexps.")
(put 'il-fun-end-regexp 'predicate 'stringp)

(defvar il-syntax-matching-quote-distance 2
  "Search limit (num of lines) for matching opening double-quote.")
(put 'il-syntax-matching-quote-distance 'predicate 'natnump)

(defvar il-timestamp-fun-column 50
  "Indent column for function timestamps.")
(put 'il-timestamp-fun-column 'predicate 'natnump)

(defvar il-tree-archive "~/.skill-mode"
  "The archive dir for the hierarchical trees created by the browser.")
(put 'il-tree-archive 'predicate 'stringp)

(defvar il-symbol-lib-version "4.3"
  "Defines which symbol library (ie Framework version) to use.
Corresponds to a directory name in the <installdir>/skill-symbol-lib dir.
To add new official (or private) libraries, see documentation on 
il-api-libraries.")
(put 'il-symbol-lib-version 'predicate 'il-existing-lib-version-p)

(defvar il-ui-remove-menus
  '(("View")("Find")("Utilities")("SPARCworks") ("SCCS"))
  "Defines menu items to be removed from the skill-mode menu-bar.
Particularly intended for XEmacs >= 19.11 which in some cases is rather
cluttered with standard menus out of the box, leaving no room for the
skill-mode menus. For more info on syntax, see documentation on
delete-menu-item.")
(put 'il-ui-remove-menus 'predicate 'listp)

;;**************************************************************
;; End of administrator options, for site-local customization.
;;**************************************************************


;;*****************************************************************
;; Font-lock setup, may be locally customized.
;; Don't fiddle with unless you are proficient with Emacs regexps.
;;*****************************************************************

(defconst il-font-lock-keywords-C-less (purecopy
 '(;;
   ;; highlight defining forms.  This doesn't work too nicely for
   ;; (defun (setf foo) ...) but it does work for (defvar foo) which
   ;; is more important.
   ;;("^(def[-a-z]+\\s +\\([^ \t\n\)]+\\)" 1 font-lock-function-name-face)
   ;;
   ;; this is highlights things like (def* (setf foo) (bar baz)), but may
   ;; be slower (I haven't really thought about it)
   ;;   ("^(def[-a-z]+\\s +\\(\\s(\\S)*\\s)\\|\\S(\\S *\\)"
   ;;("^procedure( *\\(\\S(\\)*" Lemacs
   ;;("^procedure( *\\(\\|\\S(*\\)" 1 font-lock-function-name-face) Gnu
   ("^procedure(\\s *\\(\\S(*\\)" 1 font-lock-function-name-face)
   ))
 "For consideration as a value of `il-font-lock-keywords' in C-style.
This does fairly subdued highlighting.")

(defconst il-font-lock-keywords-C-more (purecopy
  (append
   il-font-lock-keywords-C-less
   '(;; Highlight control structures
     ("[ \t\n]\\(cond\\|if\\|when\\|unless\\|case\\|caseq\\|return\\|decode\\)[(]" . 1)
     ("[ \t\n]\\(foreach\\|forall\\|for\\|while\\|let\\|labels\\|prog\\)[(]" . 1)
     ("[ \t\n]\\(then\\|else\\)[ \t\n]" . 1)
     ;;("[ \t\n]\\(return\\)[ \t\n]" . 1)
     )))
  "For consideration as a value of `il-font-lock-keywords' in C-style.
This does a lot more highlighting.")

(defconst il-font-lock-keywords-lisp-less (purecopy
 '(;;
   ;; highlight defining forms.  This doesn't work too nicely for
   ;; (defun (setf foo) ...) but it does work for (defvar foo) which
   ;; is more important.
   ;;("^(def[-a-z]+\\s +\\([^ \t\n\)]+\\)" 
   ;;   1 font-lock-function-name-face)
   ;;
   ;; this is highlights things like (def* (setf foo) (bar baz)), but may
   ;; be slower (I haven't really thought about it)
   ("^(def[-a-z]+\\s +\\(\\s(\\S)*\\s)\\|\\S(\\S *\\)"
   ;;("^(defun \\s-*[A-Za-z0-9_]+"
    1 font-lock-function-name-face)
   ))
 "For consideration as a value of `il-font-lock-keywords' in lisp-style.
This does fairly subdued highlighting.")

(defconst il-font-lock-keywords-lisp-more (purecopy
  (append
   il-font-lock-keywords-lisp-less
   '(;; Highlight control structures
     ("[(]\\(cond\\|if\\|when\\|unless\\|case\\|caseq\\|return\\|decode\\)[ \t\n]" . 1)
     ("[(]\\(for\\|foreach\\|forall\\|while\\|let\\|labels\\|prog?\\)[ \t\n]" . 1)
     ("[ \t\n]\\(then\\|else\\)[ \t\n]" . 1)
     )))
  "For consideration as a value of `il-font-lock-keywords' in lisp-style.
This does a lot more highlighting.")


;;**********************************************************
;; Minor-mode options, for user customization.
;;**********************************************************

(defvar il-arghelp_interactive-key-args t
  "*Defines how to process `key' arguments. Arghelp-mode.
If t, interactively prompts for key args in the minibuffer.
If nil, no special treatment of key args.  
The start/end of the prompting session is signalled by a twin-beep.
Press RET to skip an argument. Compulsory args are then inserted as 
empty entries, while optional args are discarded. 
Press SPC to get local var completion for symbol entries.
NOTE ! 
Unless input is a local var, strings and symbols are automatically quoted
by the input reader, list input is quoted with parentheses.")
(put 'il-arghelp_interactive-key-args 'predicate 'il-t-or-nil-p)

(defvar il-arghelp_in-focus t
  "*Defines where to display the argument help. Arghelp-mode.
t displays arghelp in a slot below point, nil displays it in the echo area.
t gives the most efficient arghelp while nil gives the least disturbance
in the focus area.")
(put 'il-arghelp_in-focus 'predicate 'il-t-or-nil-p)

(defvar il-arghelp_muted-funs '(if car cdr set setq)
  "*Functions with muted argument help. Arghelp-mode.")
(put 'il-arghelp_muted-funs 'predicate 'listp)

(defvar il-arghelp_prototype-make t
  "*Prompts for prototyping of unknown function calls. Arghelp-mode.
If t, interactively prompts for prototyping of unknown fun calls, including 
formal args and document string. If you decline they are recorded as temporary
funs. If nil, they are silently recorded as temporary funs.")
(put 'il-arghelp_prototype-make 'predicate 'il-t-or-nil-p)


(defvar il-complete_trigger-basic-fun 4
  "*Complete this pattern length to a basic function. Complete-mode.
Set to zero to disable.")
(put 'il-complete_trigger-basic-fun 'predicate 'natnump)
(defvar il-complete_trigger-api-fun 6
  "*Complete this pattern length to an API function. Complete-mode.
Set to zero to disable.")
(put 'il-complete_trigger-api-fun 'predicate 'natnump)
(defvar il-complete_trigger-local-var 3
  "*Complete this pattern length to a local variable. Complete-mode.
Set to zero to disable.")
(put 'il-complete_trigger-local-var 'predicate 'natnump)
(defvar il-complete_trigger-local-fun 5
  "*Complete this pattern length to a local function. Complete-mode.
Set to zero to disable.")
(put 'il-complete_trigger-local-fun 'predicate 'natnump)


(defvar il-font-lock_more-hilite nil
  "*Increase the degree of high-lighting. Font-lock mode.
Slows down fontifying and buffer redisplay notably. 
We strongly recommend using a speedup-method together with this one.
The behaviour is defined by the variables il-font-lock-keywords-C-more
and il-font-lock-keywords-lisp-more, and as default the control 
expressions are hilited.")
(put 'il-font-lock_more-hilite 'predicate 'il-t-or-nil-p)

(defvar il-font-lock_use-colour t
  "*Highlight with colors instead of font variations. Font-lock mode.
Cannot be turned off interactively in GnuEmacs.")
(put 'il-font-lock_use-colour 'predicate 'il-t-or-nil-p)

(defvar il-font-lock_speedup-method 'turn-on-lazy-lock
  "*Method of speeding up font-lock mode. Font-lock mode.
Lazy-lock fontifies only the visible window-region.
Fast-lock saves away font-lock caches in separate cache files which are
autoloaded the next time the source-file is visited.
Set to one of: nil, turn-on-lazy-lock, turn-on-fast-lock")
(put 'il-font-lock_speedup-method 'predicate 'il-speedup-method-or-nil-p)


(defvar il-indent_newline-at-then&else t
  "*Auto-insert newline after `then' and `else'. Indent-mode.")
(put 'il-indent_newline-at-then&else 'predicate 'il-t-or-nil-p)

(defvar il-indent_newline-at-end-of-cexp t
  "*Auto-insert newline(s) around End-of-Control-expr. Indent-mode.
Enforces uniform layout and high visibility of the End-of-Cexp.
Ensures printing of end-comments.")
(put 'il-indent_newline-at-end-of-cexp 'predicate 'il-t-or-nil-p)


(defvar il-declare_use-let-clause t
  "*Use let-clause to declare local variables. Declare-mode.
Set to nil to use prog-clause.")
(put 'il-declare_use-let-clause 'predicate 'il-t-or-nil-p)

(defvar il-declare_fill-column 75
  "*Defines the max permitted column for the local variable list.
Set to 1 for one variable per line, if you want to document each 
variable separately. Declare-mode.")
(put 'il-declare_fill-column 'predicate 'natnump)


(defvar il-tree_search-depth 4
  "*Depth searched by the hierarchy browser. Tree-mode.
The setting is a trade-off between survey, resolution and scan-times. 
Re-scan is triggered by:
  a) adding il-tree_rescan-trigger number of new funs  (at next visit)
  b) changing this option  (at next hierarchical move)
Be conservative about modifying permanently; it will make your
accumulated tree archive outdated and force re-scan of each file at
next visit. Functions not part of the main tree are listed at the end under
`UNREFERENCED FUNCTIONS'. Truncated branches are marked by trailing dots
and listed at the end under `UNREFERENCED & TRUNCATED FUNCTIONS'.")
(put 'il-tree_search-depth 'predicate 'natnump)

(defvar il-tree_indent 5
  "*Indent function hierarchy trees to this value. Tree-mode.
Used by all commands that either displays or writes hierarchy trees.")
(put 'il-tree_indent 'predicate 'natnump)

(defvar il-tree_window-height 0.15
  "*Relative height of browser window as a fraction of total screen height.
For example, a value of 0.1 implies one tenth of the total screen height.
Appropriate range is somewhere between 0.1 and 0.20. Note that window will
be deleted if the computed height is less than window-min-height. Tree-mode.")
(put 'il-tree_window-height 'predicate 'floatp)

(defvar il-tree_rescan-trigger 5
  "*Defers re-scan until N new functions has been added. Tree-mode.
The trigger condition is checked when the working file is loaded.
Newly created functions are accumulated under `APPENDED FUNCTIONS:'.")
(put 'il-tree_rescan-trigger 'predicate 'natnump)


;;**********************************************************
;; Regular options, for user customization.
;;**********************************************************

(defvar il-comment-end-of-cexp-limit 5
  "*Controls printing of end-comments for control expressions.
If control expression spans more than n lines and the closing parenthesis is
written on a separate line, an end-comment is printed when indenting the line.
Set to a high number to disable.")
(put 'il-comment-end-of-cexp-limit 'predicate 'natnump)

(defvar il-comment-fill-column 60
  "*Column beyond which automatic line-wrapping should happen.
Triggers auto-fill for every all-comment line in the file,
i.e. NOT for end-of-line comments. 
Set to a high number to disable.")
(put 'il-comment-fill-column 'predicate 'natnump)


(defconst completion-ignore-case t
  "*Tells the basic completion primitive to ignore case in user input.
Might be better off disabled, if you are troubled by accidental triggering 
in auto-complete mode.
NOTE1 ALL completion commands are affected, including filename completion
in the minibuffer, since this is a system global variable that affects
ALL buffers. 
NOTE2 Skill-mode unconditionally sets this var to t, thus overriding the
system default nil or any user setting in the .emacs file. It can only
be changed interactively, during the current session.
NOTE3 In Lucid-Emacs it is a buffer local variable. ")


(defvar il-complete-parent-local-depth 1
  "*Default upward max depth used by il-complete-parent-local-var.")
(put 'il-complete-parent-local-depth 'predicate 'natnump)

(defvar il-complete-carrier-search nil
  "*Search for carriers when searching for global variables.
`carrier' means a global var that carries info between local funs.
Slows down the search for global vars. Not recommended for large files.")
(put 'il-complete-carrier-search 'predicate 'il-t-or-nil-p)


(defvar il-header-author-name 
  (format "%s  <%s@%s>" (user-full-name) (user-login-name) (system-name))
  "*Author name for the file header. Must be a string.")
(put 'il-header-author-name 'predicate 'stringp)

(defvar il-header-user-name (user-login-name)
  "*User name for the file header. Must be a string.")
(put 'il-header-user-name 'predicate 'stringp)




(defvar il-hierarchy-down-depth 2
  "*Default downward depth used by il-hierarchy-list-down.
Set to 1 or 2 if you want il-hierarchy-list-down to be quick.")
(put 'il-hierarchy-down-depth 'predicate 'natnump)


(defvar il-indent-body 3
  "*Indent control expressions (if,cond,foreach,...) this value.")
(put 'il-indent-body 'predicate 'natnump)

(defvar il-indent-fun-body 2
  "*Indent function bodies (defun/procedure,prog/let) this value.")
(put 'il-indent-fun-body 'predicate 'natnump)

(defvar il-indent-follow-first-arg-limit 20
  "*Follow first arg if length of fun name is less than value.
Otherwise indent according to il-indent-body. Following first arg is neat but
less suitable for long function names in combination with large nesting depths.
Set to a high number to activate unconditionally.")
(put 'il-indent-follow-first-arg-limit 'predicate 'natnump)


(defvar il-move-descend-ignore-old-edit t
  "*If t, ignore old edit points and instead goto beginning of fun.
If nil, old edit points come third in priority (see il-move-descend).")
(put 'il-move-descend-ignore-old-edit 'predicate 'il-t-or-nil-p)

(defvar il-move-function-menu t
  "*Add a `jump-table of functions' menu to the menubar.
Provides direct access to every function in the file. Slows down find-file 
slightly. A mark is set at the old position, so you may return with C-u C-SPC.
In XEmacs the menu can be popped-up with Shift-button3 or F8.

Uses the standard packages func-menu (XEmacs) and imenu (GnuEmacs), which
are both highly customizable. See the sources for further details.
In XEmacs, skill-mode sets fume-menubar-menu-name,fume-scanning-message and
fume-rescanning-message. For performance fume-scanning-message is nil and
auto-rescan turned off (locally) why rescan must be done manually with the 
'Rescan buffer' menu-command.")
(put 'il-move-function-menu 'predicate 'il-t-or-nil-p)

(defvar il-move-search-path "../*/*.il ../*.il"
  "*Directories and file names to search for unknown functions.
May be absolute or relative paths (ie relative to the current buffer-file)
Used by il-move-descend when searching the file system.")
(put 'il-move-search-path 'predicate 'stringp)


(defvar il-space-append-fun nil
  "*In C-style, append a space to every function call.")
(put 'il-space-append-fun 'predicate 'il-t-or-nil-p)

(defvar il-space-append-cexp t
  "*In C-style, append a space to every control expression.")
(put 'il-space-append-cexp 'predicate 'il-t-or-nil-p)

(defvar il-space-append-var t
  "*Append a space to every successful variable completion.")
(put 'il-space-append-var 'predicate 'il-t-or-nil-p)

(defvar il-space-before-operator t
  "*Insert a space before operator if non-existing.
Applies to assignment,boolean and relational operators that are
formed by the following keys:  \" = < > & | ! ^ \".
Legal combinations of these keys are automatically merged when typing ahead.
For example, typing two successive '=' yields \"==\".")
(put 'il-space-before-operator 'predicate 'il-t-or-nil-p)

(defvar il-space-rhs-follow-lhs t
  "*Make right hand side spacing of operator follow the left hand.
Applies for the following keys: \" ) = <  > & | ! ^ \". 
The closing parenthesis spacing follows the matching opening parenthesis's,
thus deleting any surplus trailing spaces.")
(put 'il-space-rhs-follow-lhs 'predicate 'il-t-or-nil-p)


(defvar il-syntax-blink-matching-quote t
  "*Blink matching quote when writing string constants.
Set to nil to turn off.")
(put 'il-syntax-blink-matching-quote 'predicate 'il-t-or-nil-p)

(defvar il-syntax-blink-time 0.2
  "*Blink time (in seconds) for matching parenthesis and double-quote.
Suitable time depends on your typing speed.")
(put 'il-syntax-blink-time 'predicate 'floatp)

(defvar il-syntax-check-parentheses t
  "*Check the parenthesis balance when saving the file.")
(put 'il-syntax-check-parentheses 'predicate 'il-t-or-nil-p)


(defvar il-timestamp-funs t
  "*Timestamp all edited functions when saving the file.
The timestamp contains date, userid and a last-edit pointer.
The pointer is used by il-move-descend.")
(put 'il-timestamp-funs 'predicate 'il-t-or-nil-p)

(defvar il-timestamp-file-header t
  "*Timestamp the standard file header when saving the file.
Requires the existence of a standard file header to be meaningful.
See also il-create-file-header and il-timestamp-restart-from-last-edit.")
(put 'il-timestamp-file-header 'predicate 'il-t-or-nil-p)

(defvar il-timestamp-restart-from-last-edit t
  "*Restart from last edit when visiting a file.
Requires that the file has previously been saved with 
il-timestamp-file-header enabled, to be meaningful.")
(put 'il-timestamp-restart-from-last-edit 'predicate 'il-t-or-nil-p)


(defvar il-ui-experienced-user nil
  "*Makes user-interface less verbose.
Suppresses the confirmation query when pushing menu toggle-buttons.")
(put 'il-ui-experienced-user 'predicate 'il-t-or-nil-p)



;;****************************************************************************
;; Global variables, possibly but not normally for site-local customization.
;;****************************************************************************

(defvar il-user-setup-home (expand-file-name "~/.skill-mode")
  "Homedir for any user specific setup.")

(defvar il-init-file (concat il-user-setup-home "/defaults.el")
  "Personal setup-file that is loaded when skill-mode is first loaded.")

(defvar il-prototype-lib (concat il-user-setup-home "/prototype_funs.el")
  "Filename where prototype funs are saved between sessions.")

(defvar il-eval-shell-command 
  (expand-file-name "~/.gateToOpus.wakeMeUp")
  "Full path to the program that loads ~/.dataForOpus into Opus.")

(setq completion-ignored-extensions
      (append '(".e" ".ile") completion-ignored-extensions))

(defconst il-top-level '--top-level--
  "Constant that represents the top level in the current buffer.")

(defvar il-header-frame-line-length 79
  "Defines the length of the frame lines in the different headers.")

(defvar il-cexp-regexp
   "\\b\\(if\\|when\\|unless\\|foreach\\|for\\|forall\\|cond\\|case\\|caseq\\|decode\\|while\\)\\b"
   "This regexp defines what functions that are regarded as control functions.")

(defconst il-symbol-word
  "[A-Z0-9_]?[A-Z0-9]?[a-z?]+\\|[A-Z0-9][A-Z0-9][A-Z0-9]"
  "Regexp used for parsing the words of a skill symbol. Each word is
 1-2 capitals followed by lowercase chars, or 3 capitals.")

(defconst il-package-prefix-regexp "\\b[A-Za-z][a-z][a-z]?[a-z]?[A-Z0-9]"
  "Regexp used for identifying package prefixes.")

(defvar il-symbol-regexp "a-zA-Z0-9_?"
  "Regexp describing a valid skill symbol pattern.")

(defvar il-timestamp-prefix ";_"
  "The comment-prefix used in function timestamps.
Must begin with a semicolon.")

(defvar skill-mode-maintainer-address "erajonj@kieras90a.ericsson.se"
  "Email address to the skill-mode maintainer.")

(defvar skill-mode-maintainer-name 
  "Jonas Jarnestrom, Stockholm Sweden."
  "Name and organization of skill-mode maintainer.")

;;; Setup for the func-menu Emacs standard-package
;;; Made by Allan Cochrane <alanc@inmos.co.uk>
;;; Additions for lisp style by etxjojm.
;;; No longer used for the initial entire-file scan,
;;; instead the fun names are derived from il-local-funs.
;;; However, still used by the post-command-hook functions.

(defconst fume-function-name-regexp-skill
  (concat
   ;;"\\(^defun(+\\s-*[A-Za-z0-9_]+(\\)"      ; only C style
   "\\(^(?defun[( ]+[A-Za-z0-9_]+\\s-*(\\)" ;C & Lisp style
   "\\|"
   ;;"\\(^[nm]?procedure(+\\s-*[A-Za-z0-9_]+(\\)"  ;only C style
   ;;"^[nm]?procedure(+\\s-*[A-Za-z0-9_]+("  ;only C style
   "\\(^(?[nm]?procedure[( ]+[A-Za-z0-9_]+\\s-*(\\)" ;C & Lisp style
   )
  "Expression to get skill procedure names")

;;****************************************************************
;; End of Global variables
;;****************************************************************

