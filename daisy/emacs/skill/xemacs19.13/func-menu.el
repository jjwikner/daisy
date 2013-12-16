;;; func-menu.el         --- Jump to a function within a buffer.
;;;
;;; David Hughes <ukchugd@ukpmr.cs.philips.nl>
;;; Last modified: David Hughes 8th December 1995
;;; Version: 2.35
;;; Keywords: tools, c, lisp
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Synched up with: Not in FSF.
;;;
;;; Installation:
;;; =============
;;; (require 'func-menu)
;;; (define-key global-map 'f8 'function-menu)
;;; (add-hook 'find-file-hooks 'fume-add-menubar-entry)
;;; (define-key global-map "\C-cl" 'fume-list-functions)
;;; (define-key global-map "\C-cg" 'fume-prompt-function-goto)
;;; (define-key global-map '(shift button3) 'mouse-function-menu)
;;; (define-key global-map '(meta  button1) 'fume-mouse-function-goto)
;;;
;;; Description:
;;; ============
;;; Suppose you have a file with a lot of functions in it. Well, this package
;;; makes it easy to jump to any of those functions. The names of the
;;; functions in the current buffer are automatically put into a popup menu,
;;; you select one of the function-names and the point is moved to that very
;;; function. The mark is pushed on the mark-ring, so you can easily go back
;;; to where you were. Alternatively, you can use enter the name of the
;;; desired function via the minibuffer which offers completing read input. In
;;; addition, the name of the function before point is optionally displayed in
;;; the modeline.
;;;
;;; Support for non X Windows versions of Emacs:
;;; ============================================
;;; This package can also be used for non X versions of Emacs. In this case,
;;; only modeline display and completing read input from the minibuffer are
;;; possible.
;;;
;;; Modes supported:
;;; ================
;;; Ada, Assembly, Bacis2, BibTex, C++, C, Dired, Ehdm, ELisp, FORTRAN, Ksh,
;;; Latex, Lelisp, Makefile, Maple, Modula2, Modula3, Outline, Pascal, Perl,
;;; Postscript, Prolog, PVS, Python, SGML, Scheme, Tcl, Verilog
;;;
;;; Acknowledgements:
;;; =================
;;;
;;; Cleanup suggestions
;;; Stig <stig@hackvan.com>
;;;
;;; Idea for jumping directly with a mouse click
;;; Marc Paquette <Marc.Paquette@Softimage.COM>
;;;
;;; Prolog mode additions based on functions for Postscript mode
;;; Laszlo Teleki <laszlo@ipb.uni-bonn.de>
;;;
;;; Idea for displaying function name in modeline
;;; Paul Filipski <filipski@blackhawk.com>
;;;
;;; Fame mode support
;;; Cooper Vertz <cooper@prod2.imsi.com>
;;;
;;; Made fume-match-find-next-function-name iterative, not recursive, to avoid
;;; blowing out the emacs stack on big files with lots of prototypes.
;;; Joe Marshall <jrm@odi.com>
;;;
;;; Verilog support
;;; Matt Sale <mdsale@icdc.delcoelect.com>
;;;
;;; Minibuffer interface & Pascal support
;;; Espen Skoglund <espensk@stud.cs.uit.no>
;;;
;;; Python support
;;; Shuichi Koga <skoga@virginia.edu>
;;;
;;; Maple support
;;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
;;;
;;; Combined Tcl and C++ function finder
;;; Andy Piper <ajp@eng.cam.ac.uk>
;;;
;;; Perl Support
;;; Alex Rezinsky <alexr@msil.sps.mot.com>
;;; Michael Lamoureux <lamour@engin.umich.edu>
;;;
;;; Suggested mouse interface
;;; Raymond L. Toy <toy@soho.crd.ge.com>
;;;
;;; Dired support
;;; Improved modula support
;;; Numerous code cleanups
;;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
;;;
;;; Makefile support
;;; Suggested multi-choice sublisting
;;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
;;;
;;; Suggestions for menubar entry
;;; Andy Piper <ajp@eng.cam.ac.uk>
;;;
;;; Ada support
;;; Scott Evans  <gse@ocsystems.com>
;;; Michael Polo <mikep@polo.mn.org> <mikep@cfsmo.honeywell.com>
;;;
;;; Scheme, BibTeX, Ehdm & PVS support
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;;
;;; Modula support
;;; Geoffrey Wyant <gwyant@cloyd.east.sun.com>
;;;
;;; SGML support; submenu indexing
;;; Thomas Plass <thomas.plass@mid-heidelberg.de>
;;;
;;; Extensions to fume-function-name-regexp-lisp
;;; Kari Heinola <kph@dpe.fi>
;;; Milo A. Chan <chan@jpmorgan.com>
;;; Cedric Beust <Cedric.Beust@sophia.inria.fr>
;;; Joachim Krumnow <krumnow@srsir02.ext.sap-ag.de>
;;;
;;; ksh support
;;; Philippe Bondono <bondono@vnet.ibm.com>
;;;
;;; FORTRAN support
;;; Paul Emsley <paule@chem.gla.ac.uk>
;;; Raymond L. Toy <toy@soho.crd.ge.com>
;;; Richard Cognot <cognot@elfgrc.co.uk>
;;; Greg Sjaardema <gdsjaar@sandia.gov>
;;;
;;; Latex support
;;; Wolfgang Mettbach <wolle@uni-paderborn.de>
;;; Paolo Frasconi <paolo@mcculloch.ing.unifi.it>
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
;;;
;;; Assembly support
;;; Bob Weiner <weiner@mot.com>
;;;
;;; Removal of cl dependencies
;;; Russell Ritchie <russell@gssec.bt.co.uk>
;;;
;;; C++ mode enhancemencements for func-menu
;;; Andy Piper      <ajp@eng.cam.ac.uk>
;;; Kevin R. Powell <powell@csl.ncsa.uiuc.edu>
;;; Mats Lidell     <mats.lidell@eua.ericsson.se>
;;; Mike Battaglia  <mbattagl@spd.dsccc.com>
;;; Oliver Schittko <schittko@fokus.gmd.de>
;;; Russell Ritchie <russell@gssec.bt.co.uk>
;;;
;;; Tcl mode additions for func-menu
;;; Andy Piper <ajp@eng.cam.ac.uk>
;;; Jean-Michel Augusto <augusto@eurecom.fr>
;;; Dr P.G. Sjoerdsma <pgs1002@esc.cam.ac.uk>
;;;
;;; Postscript mode additions for func-menu
;;; Leigh Klotz <klotz@adoc.xerox.com>
;;;
;;; Suggestions for popup menu positioning
;;; Marc Gemis <makke@wins.uia.ac.be>
;;;
;;; Original FSF package
;;; Ake Stenhoff <etxaksf@aom.ericsson.se>

;;; Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;  Environment Initialisation  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst fume-version "2.35")

(defconst fume-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defmacro fume-defvar-local (var value &optional doc)
  "Defines SYMBOL as an advertised variable.
Performs a defvar, then executes `make-variable-buffer-local' on
the variable.  Also sets the `permanent-local' property, so that
`kill-all-local-variables' (called by major-mode setting commands)
won't destroy func-menu control variables."
  (` (progn
       (if (, doc)
           (defvar (, var) (, value) (, doc))
         (defvar (, var) (, value)))
       (make-variable-buffer-local '(, var))
       (put '(, var) 'permanent-local t))))

(byte-compiler-options
  (optimize t)
  (new-bytecodes t)
  (warnings (- free-vars unresolved)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  Backward compatibility hacks for older versions of XEmacs  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(or (fboundp 'selected-frame)
    (defalias 'selected-frame 'selected-screen))

(if (fboundp 'locate-window-from-coordinates)
    ;; Older versions of XEmacs need a more robust version of 'event-window'
    (defun fume-event-window (event)
      (or (event-window event)
          (locate-window-from-coordinates
           (selected-frame) (list (event-x event) (event-y event)))
          (locate-window-from-coordinates
           (selected-frame) (list (event-x event) (1- (event-y event))))))
  ;; In post 19.11 versions of XEmacs 'event-window' now works acceptably
  (defalias 'fume-event-window 'event-window))

(defconst fume-modeline-buffer-identification
  (if (boundp 'modeline-buffer-identification)
      'modeline-buffer-identification
    'mode-line-buffer-identification))

(defconst fume-use-local-post-command-hook
  (boundp 'local-post-command-hook))

(cond ((fboundp 'add-submenu)
       (defconst fume-add-submenu 'add-submenu)
       (defun fume-munge-menu-args (menu-name submenu before)
         (list nil (cons menu-name submenu) before)))
      (t
       (defconst fume-add-submenu 'add-menu)
       (defun fume-munge-menu-args (menu-name submenu before)
         (list nil menu-name submenu before))))

(defun fume-add-submenu (menu-name submenu before)
  (apply fume-add-submenu (fume-munge-menu-args menu-name submenu before)))

(defconst fume-not-tty
  (or (and (fboundp 'device-type) (not (eq 'tty (device-type))))
      (and (symbol-value 'window-system) t))) ; obsolete test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;  Customizable Variables  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fume-auto-position-popup t
  "*Set to nil if you don't want the menu to appear in the corner of the window
in which case it will track the mouse position instead.")

(fume-defvar-local fume-display-in-modeline-p t
  "*Set to nil if you don't want the function name appearing in the modeline.
If your modeline is already full, then you can set this variable to something
besides nil or t and the current function will replace the normal
modeline-buffer-identification

Note, this is a buffer-local variable.")

(defvar fume-buffer-name "*Function List*"
  "Name of buffer used to list functions when fume-list-functions called")

(fume-defvar-local
 fume-menubar-menu-name "Functions"
 "*Set this to the string you want to appear in the menubar")

(defvar fume-menubar-menu-location "Tools"
  "*Set this nil if you want the menu to appear last on the menubar.
Otherwise set this to the menu you want \"Functions\" to appear in front of.")

(defvar fume-max-items 24
  "*Maximum number of elements in a function (sub)menu.")

(defvar fume-fn-window-position 3
  "*Number of lines from top of window at which to show function.")

(defvar fume-index-method 3
  "*Set this to the method number you want used.

Methods currently supported:
0 = if you want submenu names to be numbered
1 = if you want submenu range indicated by first character
2 = if you want submenu range indicated by first 12 characters
3 = if you want submenu range indicated by as many characters as needed")

(defvar fume-scanning-message "Scanning buffer... (%3d%%)"
  "*Set to nil if you don't want progress messages during manual scanning
of the buffer.")

(defvar fume-rescanning-message nil
  "*Set to non-nil if you want progress messages during automatic scanning
of the buffer. For example \"Re-Scanning buffer...\"")

(defvar fume-rescan-trigger-counter-buffer-size 10000
  "Used to tune the frequency of automatic checks on the buffer.
The function fume-rescan-buffer-trigger only works whenever the value of the
variable fume-rescan-trigger-counter reaches zero, whereupon it gets reset to
buffer-size/fume-rescan-trigger-counter-buffer-size.")

(fume-defvar-local
 fume-sort-function 'fume-sort-by-name
 "*The function to use for sorting the function menu.

Set this to nil if you don't want any sorting (faster).
The items in the menu are then presented in the order they were found
in the buffer.

The function should take two arguments and return T if the first
element should come before the second.  The arguments are cons cells;
(NAME . POSITION).  Look at 'fume-sort-by-name' for an example.")

(fume-defvar-local
 fume-rescan-buffer-hook nil
 "*Buffer local hook to call at the end of each buffer rescan")

;;; This hook is provided for outl-mouse and must not be made buffer local as
;;; this appears to break outl-mouse for some reason.
;;;
(defvar fume-found-function-hook nil
  "*Hook to call after every function match.")

;;; Idea for jumping directly with a mouse click
;;; Marc Paquette <Marc.Paquette@Softimage.COM>
;;;
(defvar fume-no-prompt-on-valid-default nil
  "*Set non-nil if 'fume-prompt-function-goto' should jump without prompting
when a valid default exists.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;  Buffer local variables  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fume-defvar-local
 fume-auto-rescan-buffer-p t
 "Buffer local variable which if non-nil permits automatic buffer rescanning
by func-menu.

Usage: By default, fume-auto-rescan-buffer-p is set to non-nil. If you feel that
a particular mode 'foo' is becoming too slow as a result of automatic rescanning
by func-menu, then do something along the lines of the following:

   (defun remove-func-menu-auto-rescan ()
      (setq fume-auto-rescan-buffer-p nil))

   (add-hook 'foo-mode-hook 'remove-func-menu-auto-rescan)")

(fume-defvar-local
 fume-funclist nil
 "The latest list of function names in the buffer")

(fume-defvar-local
 fume-function-name-regexp nil
 "The keywords to show in a menu")

(fume-defvar-local
 fume-find-next-function-name-method nil
 "The function to use to find the next function name in the buffer")

(fume-defvar-local
 fume-modeline-funclist nil
 "The latest list of function names in the buffer to display in the modeline")

(fume-defvar-local
 fume-funclist-dirty-p nil
 "Flags whether the buffer is in need of a fresh scan")

(fume-defvar-local
 fume-rescan-inhibit-p nil
 "Internal variable only. DO NOT TOUCH.")

(fume-defvar-local
 fume-rescan-trigger-counter 0
 "Used in large buffers to optimise checking frequency")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;  Mode specific regexp's and hooks  ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Every fume-function-name-regexp-<language> should uniquely identify a
;;; function for that certain language.

;;; Lisp
;;;
;;; Cedric Beust <Cedric.Beust@sophia.inria.fr>
(defconst fume-function-name-regexp-lisp
  (concat
   "\\(^(defun+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
   "\\|"
   "\\(^(defsubst+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
   "\\|"
   "\\(^(defmacro+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
   "\\|"
   "\\(^(de+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
   "\\|"
   "\\(^(dmd+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
   )
  "Expression to get lisp function names")

;;; C
;;;
;;; Danny Bar-Dov <danny@acet02.amil.co.il>
(defconst fume-function-name-regexp-c
  (concat
   "^[a-zA-Z0-9]+\\s-?"                ; type specs; there can be no
   "\\([a-zA-Z0-9_*]+\\s-+\\)?"        ; more than 3 tokens, right?
   "\\([a-zA-Z0-9_*]+\\s-+\\)?"
   "\\([*&]+\\s-*\\)?"                 ; pointer
   "\\([a-zA-Z0-9_*]+\\)[ \t\n]*("     ; name
   )
  "Expression to get C function names")

;;; C++
;;;
;;; Andy Piper      <ajp@eng.cam.ac.uk>
;;; Kevin R. Powell <powell@csl.ncsa.uiuc.edu>
;;; Mats Lidell     <mats.lidell@eua.ericsson.se>
;;; Mike Battaglia  <mbattagl@spd.dsccc.com>
;;; Oliver Schittko <schittko@fokus.gmd.de>
(defconst fume-function-name-regexp-c++
  (cons
   (concat
    "^\\(template\\s +<[^>]+>\\s +\\)?"          ; template formals
    "\\([a-zA-Z0-9_*&<,>:]+\\s-+\\)?"            ; type specs; there can be no
    "\\([a-zA-Z0-9_*&<,>\"]+\\s-+\\)?"           ; more than 3 tokens, right?
    "\\([a-zA-Z0-9_*&<,>]+\\s-+\\)?"
    "\\(\\([a-zA-Z0-9_~:<,>*]\\|\\(\\s +::\\s +\\)\\)+\\)"
    "\\(o?perator\\s *.[^(]*\\)?\\(\\s-\\|\n\\)*(" ; name
    ) 5)
  "Expression to get C++ function names")

;;; FORTRAN
;;;
;;; Paul Emsley <paule@chem.gla.ac.uk>
;;; Raymond L. Toy <toy@soho.crd.ge.com>
;;; Richard Cognot <cognot@elfgrc.co.uk>
;;; Greg Sjaardema <gdsjaar@sandia.gov>
(defconst fume-function-name-regexp-fortran
  (concat
   ;; >= six spaces
   "^      \\s-*"
   ;; type specs
   "+[a-zA-Z0-9*]*\\s-*"
   ;; continuation lines
   "\\(\n     [^ 0]\\s-*\\)*"
   ;; function or subroutine
   "\\(entry\\|ENTRY\\|function\\|FUNCTION\\|subroutine\\|SUBROUTINE\\)\\s-*"
   ;; continuation lines
   "\\(\n     [^ 0]\\s-*\\)*"
   )
  "Expression to get fortran function and subroutine names")

;;; Modula
(defconst fume-function-name-regexp-modula
  "^\\s-*PROCEDURE\\s-+[A-Za-z0-9_-]+"
  "Expression to get Modula function names")

;;; Bacis2
;;;
;;; CV MEDUSA's 4th generation language
(defconst fume-function-name-regexp-bacis
  "module_define(!\\|define_constant(!\\|sys_sysdefine(!\\|<<dbgid +\\s-*"
  "Expression to get Bacis2 function names")

;;; Maple
;;;
;;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
(defvar fume-function-name-regexp-maple
  "^[ \t]*[a-zA-Z0-9_]+[ \t]*:=[ \t]*proc[ \t]*("
  "Expression to get maple function/procedure names")

;;; Tcl
;;;
;;; Andy Piper <ajp@eng.cam.ac.uk>
;;; Jean-Michel Augusto <augusto@eureecom.fr>
;;; Dr P.G. Sjoerdsma <pgs1002@esc.cam.ac.uk>
(defconst fume-function-name-regexp-tcl
  (cons "^\\s *proc\\s +\\(\\S-+\\)\\s *{" 1)
  "Expression to get Tcl function Names")

;;; Perl
;;;
;;; Alex Rezinsky <alexr@msil.sps.mot.com>
;;; Michael Lamoureux <lamour@engin.umich.edu>
(defconst fume-function-name-regexp-perl "^sub[ \t]+\\([A-Za-z0-9_]+\\)"
  "Expression to get Perl function Names")

;;; Python support
;;; Shuichi Koga <skoga@virginia.edu>
;;;
(defconst fume-function-name-regexp-python
  "^\\s-*\\(class\\|def\\)+\\s-*\\([A-Za-z0-9_]+\\)\\s-*[(:]"
  "Expression to get Python class and function names")

;;; Postscript
;;;
;;; Leigh L. Klotz <klotz@adoc.xerox.com>
(defconst fume-function-name-regexp-postscript
  "^/[^][ \t{}<>]*"
  "Expression to get postscript function names")

;;; Prolog
;;;
;;; Laszlo Teleki <laszlo@ipb.uni-bonn.de>
(defconst fume-function-name-regexp-prolog
  "^[a-z][a-zA-Z0-9_]+"
  "Expression to get prolog fact and clause names")

;;; Ehdm
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defconst fume-function-name-regexp-ehdm
  (concat
   "[A-Za-z0-9_]*:[ ]*"
   "\\([Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]\\|"
   "[Ll][Ee][Mm][Mm][Aa]\\|"
   "[Aa][Xx][Ii][Oo][Mm]\\|"
   "[Pp][Rr][Oo][Vv][Ee]\\|"
   "[Tt][Hh][Ee][Oo][Rr][Ee][Mm]"
   "\\)"
   )
  "*Expression to get Ehdm function, theorems, axioms, lemmas, and proofs.")

;;; PVS
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defconst fume-function-name-regexp-pvs
  (concat
   "\\([A-Za-z0-9_]*:[ ]*"
   "\\([Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]\\|"
   "[Ll][Ee][Mm][Mm][Aa]\\|"
   "[Aa][Xx][Ii][Oo][Mm]\\|"
   "[Tt][Hh][Ee][Oo][Rr][Ee][Mm]\\|"
   "[Ff][Or][Rr][Mm][Uu][La][Aa]"
   "\\|"
   "\\[.*\\]"
   "\\)\\)\\|"
   "[A-Za-z0-9_]*(.*)[ ]*:"
   )
  "*Expression to get PVS functions, theorems, axioms, lemmas")

;;; Tex, LaTex
;;;
;;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
;;; Paolo Frasconi <paolo@mcculloch.ing.unifi.it>
(fume-defvar-local fume-tex-chapter 0)
(fume-defvar-local fume-tex-section 0)
(fume-defvar-local fume-tex-subsection 0)
(fume-defvar-local fume-tex-subsubsection 0)

(defun fume-tex-rescan-buffer-hook ()
  (setq fume-tex-chapter 0
        fume-tex-section 0
        fume-tex-subsection 0
        fume-tex-subsubsection 0))

(defun fume-tweak-tex-mode ()
  (setq fume-sort-function nil)
  (add-hook 'fume-rescan-buffer-hook 'fume-tex-rescan-buffer-hook))

(add-hook 'tex-mode-hook 'fume-tweak-tex-mode)
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(add-hook 'TeX-mode-hook 'fume-tweak-tex-mode)
;;; Wolfgang Mettbach <wolle@uni-paderborn.de>
(add-hook 'latex-mode-hook 'fume-tweak-tex-mode)
(add-hook 'LaTeX-mode-hook 'fume-tweak-tex-mode)

;;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
(defconst fume-section-name-regexp-latex
  (concat
   "^\\s-*\\\\\\("
   "\\(sub\\)*section\\|chapter\\)"
   "\\*?\\(\\[[^]]*\\]\\)?{\\([^}]*\\)}"
   )
  "Expression to get latex section names")

;;; ksh
;;;
;;; Philippe Bondono <bondono@vnet.ibm.com>
(defconst fume-function-name-regexp-ksh
  (concat
   "\\(^\\s-*function\\s-+[A-Za-z_][A-Za-z_0-9]*\\)"
   "\\|"
   "\\(^\\s-*[A-Za-z_][A-Za-z_0-9]*\\s-*()\\)")
  "Expression to get ksh function names")

;;; Scheme
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defconst fume-function-name-regexp-scheme
  "^(define [ ]*"
  "Expression to get Scheme function names")

;;; BibTeX
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defconst fume-function-name-regexp-bibtex
  "^@[A-Za-z]*[({]\\([A-Za-z0-9:;&-]*\\),"
  "Expression to get bibtex citation headers.")

;;; SGML
;;;
;;; Thomas Plass <thomas.plass@mid-heidelberg.de>
(defconst fume-function-name-regexp-sgml
  "<!\\(element\\|entity\\)[ \t\n]+%?[ \t\n]*\\([A-Za-z][-A-Za-z.0-9]*\\)"
  "Expression to find declaration of SGML element or entity")

;;; Ada
;;;
;;; Michael Polo <mikep@polo.mn.org> <mikep@cfsmo.honeywell.com>
(defconst fume-function-name-regexp-ada
  (cons "^[ \t]*\\(procedure\\|PROCEDURE\\|function\\|FUNCTION\\)[ \n\t]+\\([a-zA-Z0-9_]+\\|\"[^\"]\"\\)" 2)
  "Expression to find declaration of Ada function")

;;; ignore prototypes, 'renames', 'is new' to eliminate clutter
;;;
;;; Scott Evans <gse@ocsystems.com>
(defconst fume-function-name-regexp-ada-ignore
  "[ \n\t]*\\(([^()]+)[ \n\t]*\\)?\\(return[ \t\n]+[^ \t\n;]+[ \n\t]*\\)?\\(;\\|is[ \n\t]+new[ \n\t]\\|renames\\)"
  "ignore if ada function name matches this string")

;;; Makefiles
;;;
;;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
(defconst fume-function-name-regexp-make
  "^\\(\\(\\$\\s(\\)?\\(\\w\\|\\.\\)+\\(:sh\\)?\\(\\s)\\)?\\)\\s *\\(::?\\|\\+?=\\)"
  "Expression to get makefile target names")
(add-hook 'makefile-mode-hook 'fume-add-menubar-entry)

;;; Directory Listings
;;;
;;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
;;; regexp stolen from font-lock-mode
(defconst fume-function-name-regexp-dired
  "^. +d.*\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) +[0-9]+ +[0-9:]+ \\(.*\\)$"
  "Expression to get directory names")

;;; Pascal
;;;
;;; Espen Skoglund <espensk@stud.cs.uit.no>
(defconst fume-function-name-regexp-pascal
  "^\\(function\\|procedure\\)[ \t]+\\([_a-zA-Z][_a-zA-Z0-9]*\\)"
  "Expression to get function/procedure names in pascal.")


;;; Fame
;;;
;;; Cooper Vertz <cooper@prod2.imsi.com>
(defconst fume-function-name-regexp-fame
  "^\\(function\\|procedure\\)[ \t]+\\([#\\$%_a-zA-Z][#\\$%_a-zA-Z0-9]*\\)"
  "Expression to get function/procedure names in fame.")


;;; Verilog
;;;
;;; Matt Sale <mdsale@icdc.delcoelect.com>
(defconst fume-function-name-regexp-verilog
  "^\\(task\\|function\\|module\\|primitive\\)[ \t]+\\([A-Za-z0-9_+-]*\\)[ \t]*(?"
  "Expression to get verilog module names")


;;; Assembly
(defconst fume-function-name-regexp-asm
"^\\([a-zA-Z_.$][a-zA-Z0-9_.$]*\\)[ \t]*:"
  "Expression to get assembly label names")

;;; This is where the mode specific regexp's are hooked in
;;;
(defconst fume-function-name-regexp-alist
  '(;; Lisp
    (emacs-lisp-mode              . fume-function-name-regexp-lisp)
    (common-lisp-mode             . fume-function-name-regexp-lisp)
    (fi:common-lisp-mode          . fume-function-name-regexp-lisp)
    (fi:emacs-lisp-mode           . fume-function-name-regexp-lisp)
    (fi:franz-lisp-mode           . fume-function-name-regexp-lisp)
    (fi:inferior-common-lisp-mode . fume-function-name-regexp-lisp)
    (fi:inferior-franz-lisp-mode  . fume-function-name-regexp-lisp)
    (fi:lisp-listener-mode        . fume-function-name-regexp-lisp)
    (lisp-mode                    . fume-function-name-regexp-lisp)
    (lisp-interaction-mode        . fume-function-name-regexp-lisp)

    ;; C
    (c-mode      . fume-function-name-regexp-c)
    (elec-c-mode . fume-function-name-regexp-c)
    (c++-c-mode  . fume-function-name-regexp-c)

    ;; C++
    (c++-mode . fume-function-name-regexp-c++)

    ;; FORTRAN
    (fortran-mode . fume-function-name-regexp-fortran)

    ;; Modula
    (modula-2-mode . fume-function-name-regexp-modula)
    (modula-3-mode . fume-function-name-regexp-modula)

    ;; Bacis2
    (bacis-mode . fume-function-name-regexp-bacis)

    ;; Maple
    (maple-mode . fume-function-name-regexp-maple)

    ;; Perl
    (perl-mode . fume-function-name-regexp-perl)

    ;; Python
    (alice-mode  . fume-function-name-regexp-python)
    (python-mode . fume-function-name-regexp-python)

    ;; Postscript
    (postscript-mode . fume-function-name-regexp-postscript)

    ;; Prolog
    (prolog-mode . fume-function-name-regexp-prolog)

    ;; Tcl
    (tcl-mode . fume-function-name-regexp-tcl)

    ;; ksh
    (ksh-mode . fume-function-name-regexp-ksh)

    ;; LaTeX
    (latex-mode . fume-section-name-regexp-latex)
    (LaTeX-mode . fume-section-name-regexp-latex)

    ;; Scheme
    (scheme-mode . fume-function-name-regexp-scheme)

    ;; BibTeX
    (bibtex-mode . fume-function-name-regexp-bibtex)

    ;; Ehdm & PVS
    (ehdm-mode . fume-function-name-regexp-ehdm)
    (pvs-mode  . fume-function-name-regexp-pvs)

    ;; SGML
    (sgml-mode . fume-function-name-regexp-sgml)

    ;; Ada
    (ada-mode . fume-function-name-regexp-ada)

    ;; Makefiles
    (makefile-mode . fume-function-name-regexp-make)

    ;; Dired
    (dired-mode . fume-function-name-regexp-dired)

    ;; Pascal
    (pascal-mode . fume-function-name-regexp-pascal)

    ;; Fame
    (fame-mode . fume-function-name-regexp-fame)

    ;; Verilog
    (verilog-mode . fume-function-name-regexp-verilog)

    ;; Assembly
    (asm-mode . fume-function-name-regexp-asm)
    )

  "The connection between a mode and the regexp that matches function names.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;  Mode specific finding functions  ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Default routine : Note, most modes will need a specialised routine
;;;
(defun fume-find-next-function-name (buffer)
  "Searches for the next function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((char (progn
                    (backward-up-list 1)
                    (save-excursion
                      (goto-char (scan-sexps (point) 1))
                      (skip-chars-forward "[ \t\n]")
                      (following-char)))))
        ;; Skip this function name if it is a prototype declaration.
        (if (and (eq char ?\;) (not (eq major-mode 'emacs-lisp-mode)))
            (fume-find-next-function-name buffer)
          ;; Get the function name and position
          (let (beg)
            (forward-sexp -1)
            (setq beg (point))
            (forward-sexp)
            (cons (buffer-substring beg (point)) beg))))))

;;; General purpose sexp find function
;;;
(defun fume-find-next-sexp (buffer)
  "Searches for the next sexp type function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (save-excursion (forward-sexp -1) (point))))
        (cons (buffer-substring beg (point)) beg))))

;;; Specialised routine to get the next ehdm entity in the buffer.
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;;
(defun fume-find-next-ehdm-entity (buffer)
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to get the next PVS entity in the buffer.
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;;
(defun fume-find-next-pvs-entity (buffer)
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (goto-char (1- end))
        (if (looking-at ":")
            (setq end (1- end)))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to get the next C function name in the buffer.
;;;
(defun fume-find-next-c-function-name (buffer)
  "Searches for the next C function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((char (progn
                    (backward-up-list 1)
                    (save-excursion
                      (goto-char (scan-sexps (point) 1))
                      (skip-chars-forward "[ \t\n]")
                      (following-char)))))
        ;; Skip this function name if it is a prototype declaration.
        (if (eq char ?\;)
            (fume-find-next-function-name buffer)
          (let (beg
                name)
            ;; Get the function name and position
            (forward-sexp -1)
            (setq beg (point))
            (forward-sexp)
            (setq name (buffer-substring beg (point)))
            ;; ghastly crock for DEFUN declarations
            (cond ((string-match "^DEFUN\\s-*" name)
                   (forward-word 1)
                   (forward-word -1)
                   (setq beg (point))
                   (cond ((re-search-forward "\"," nil t)
                          (re-search-backward "\"," nil t)
                          (setq name
                                (format "%s %s"
                                        name
                                        (buffer-substring beg (point))))))))
            ;; kludge to avoid 'void' in menu
            (if (string-match "^void\\s-*" name)
                (fume-find-next-function-name buffer)
              (cons name beg)))))))

;;; <jrm@odi.com>
;;; <ajp@eng.cam.ac.uk>
;;; <schittko@fokus.gmd.de>
;;;
(defun fume-match-find-next-function-name (buffer)
  "General next function name in BUFFER finder using match.
The regexp is assumed to be a two item list the car of which is the regexp to
use, and the cdr of which is the match position of the function name."
  (set-buffer buffer)
  (let ((result nil)
        (continue t))
    (while continue
      ;; Search for the function
      (if (re-search-forward (car fume-function-name-regexp) nil t)
          (let ((char (progn
                        (backward-up-list 1)
                        (save-excursion
                          (goto-char (scan-sexps (point) 1))
                          (following-char)))))
            ;; Skip this function name if it is a prototype declaration.
            (if (eq char ?\;)
                nil
              (setq result
                    ;; Get the function name and position including scope
                    (cons (buffer-substring
                           (match-beginning (cdr fume-function-name-regexp))
                           (point))
                          (match-beginning (cdr fume-function-name-regexp)))
                    continue nil)))
        (setq continue nil)))
    result))

;;; Specialised routine to find the next Perl function
;;;
(defun fume-find-next-perl-function-name (buffer)
  "Searches for the next Perl function in BUFFER."
  (fume-find-next-sexp buffer))

;;; Specialised routine to find the next Python function
;;; Shuichi Koga <skoga@virginia.edu>
;;;
(defun fume-find-next-python-function-name (buffer)
  "Searches for the next python function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (save-excursion
        (let* ((retpnt (match-beginning 2))
               (retname (buffer-substring retpnt (match-end 2))))
          (goto-char (match-beginning 0))
          (cond ((looking-at "\\s-+def")
                 (re-search-backward
                  "^class\\s-*\\([A-Za-z0-9_]+\\)\\s-*[(:]" nil t)
                 (setq retname
                       (concat
                        (buffer-substring (match-beginning 1) (match-end 1))
                        "."
                        retname))))
          (cons retname retpnt)))))

;;; Specialised routine to find the next Modula function or subroutine.
;;;
(defun fume-find-next-modula-function-name (buffer)
  "Searches for the next modula function in BUFFER."
  (fume-find-next-sexp buffer))

;;; Specialised routine to find the next directory.
;;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
;;;
(defun fume-find-next-directory-name (buffer)
  "Searches for the next directory in dired BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to find the next FORTRAN function or subroutine
;;;
(defun fume-find-next-fortran-function-name (buffer)
  "Searches for the next fortran function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((pos (point))
            ;; name may have "_" but must start with a letter
            (name-regexp "\\s-+[a-zA-Z]+[_a-zA-Z0-9*]*")
            (eol (save-excursion (end-of-line 1) (point))))
        (skip-chars-backward " \t")
        (if (re-search-forward name-regexp eol t)
            ;; name is ok; so return it
            (cons (buffer-substring pos (point)) pos)
          ;; rubbish found; skip to next function
          (fume-find-next-fortran-function-name buffer)))))

;;; Specialised routine to get the next postscript function name in the buffer
;;; Leigh L. Klotz <klotz@adoc.xerox.com>
;;;
(defun fume-find-next-postscript-function-name (buffer)
  "Searches for the next postscript function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (beginning-of-line 1) (point))))
        (forward-sexp)
        ;; keep including sexps as long as they
        ;; start with / or [.
        (if (looking-at "\\s-+\\(/\\|\\[\\)")
            (forward-sexp))
        (cons (buffer-substring beg (point)) beg))))

;;; Specialised routine to get the next prolog fact/clause name in the buffer
;;; Laszlo Teleki <laszlo@ipb.uni-bonn.de>
;;;
(defun fume-find-next-prolog-function-name (buffer)
  "Searches for the next prolog fact or clause in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (beginning-of-line 1) (point))))
        (forward-sexp)
        (cons (buffer-substring beg (point)) beg))))

;;; Specialised routine to get the next bacis2 procedure name in the buffer
;;;
(defun fume-find-next-bacis-function-name (buffer)
  "Searches for the next Bacis2 function in BUFFER"
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((pos (point))
            (name (condition-case ()
                      (funcall
                       (symbol-function (intern "focus-get-function-name")))
                    (error nil))))
        (if (null name)
            (fume-find-next-bacis-function-name buffer)
          ;; jump past possible function dbgid
          (re-search-forward
           (format "<<dbgid +\\s-*%s%s" name "\\s-*>>") nil t)
          (cons name pos)))))

;;; Specialized routine to get the next Maple function name in the buffer
;;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
;;;
(defun fume-find-next-maple-function-name (buffer)
  "Searches for the next maple function in BUFFER"
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (backward-up-list 1) (forward-sexp -2) (point))))
        (forward-sexp)
        (cons (buffer-substring beg (point)) beg))))

;;; Specialised routine to get the next latex section name in the buffer
;;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
;;; Paolo Frasconi <paolo@mcculloch.ing.unifi.it>
;;;
(defun fume-find-next-latex-section-name (buffer)
  "Searches for the next latex section in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let* ((secname (buffer-substring (match-beginning 1) (match-end 1)))
             (beg (match-beginning 4))
             (name (buffer-substring beg (match-end 4))))
        (cond ((string= secname "chapter")
               (setq fume-tex-chapter (1+ fume-tex-chapter)
                     fume-tex-section 0
                     fume-tex-subsection 0
                     fume-tex-subsubsection 0
                     name (concat fume-tex-chapter " " (upcase name))))
              ((string= secname "section")
               (setq fume-tex-section (1+ fume-tex-section)
                     name (concat
                           (if (> fume-tex-chapter 0)
                               (concat fume-tex-chapter ".") "")
                           fume-tex-section " " name)
                     fume-tex-subsection 0
                     fume-tex-subsubsection 0))
              ((string= secname "subsection")
               (setq fume-tex-subsection (1+ fume-tex-subsection)
                     name (concat
                           (if (> fume-tex-chapter 0)
                               (concat fume-tex-chapter ".") "")
                           fume-tex-section "."
                           fume-tex-subsection " " name)
                     fume-tex-subsubsection 0))
              ((string= secname "subsubsection")
               (setq fume-tex-subsubsection (1+ fume-tex-subsubsection)
                     name (concat
                           (if (> fume-tex-chapter 0)
                               (concat fume-tex-chapter ".") "")
                           fume-tex-section "."
                           fume-tex-subsection "."
                           fume-tex-subsubsection " " name)))
              ((string= secname "subsubsection")
               (setq name (concat "   " name))))
        (cons name beg))))

;;; Specialised routine to get the next ksh function in the buffer
;;; Philippe Bondono <bondono@vnet.ibm.com>
;;;
(defun fume-find-next-ksh-function-name (buffer)
  "Searches for the ksh type function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let (name
            (beg (match-beginning 0)))
        (cond ((re-search-backward "\\(^\\|\\s-\\)function\\s-" beg t)
               (re-search-forward
                "\\(function\\s-+\\)\\([A-Za-z_][A-Za-z_0-9]*\\)" nil t)
               (setq beg (match-beginning 2)
                     name (buffer-substring beg (match-end 2))))
              (t
               (re-search-backward
                "\\(^\\|\\s-\\)\\([A-Za-z_][A-Za-z_0-9]*\\)" beg t)
               (setq beg (match-beginning 2)
                     name (buffer-substring beg (match-end 2)))))
        (if (null name)
            (fume-find-next-ksh-function-name buffer)
          (end-of-line)
          (cons name beg)))))

;;; Specialised routine to get the next Scheme function in the buffer
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;;
(defun fume-find-next-scheme-function (buffer)
  "Searches for the next Scheme function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (if (looking-at "(") (forward-char 1)) (point)))
            (end (save-excursion (forward-sexp) (point))))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to get the next BibTeX citation in the buffer
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;;
(defun fume-find-next-bibtex-citation (buffer)
  "Searches for the next BibTeX citation in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to get the next SGML declaration in the buffer
;;; Thomas Plass <thomas.plass@mid-heidelberg.de>
;;;
(defun fume-find-next-sgml-element-name (buffer)
  "Searches for the next SGML declaration in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((type (buffer-substring (match-beginning 1) (match-end 1)))
            (beg (match-beginning 2))
            (name (buffer-substring (match-beginning 2) (match-end 2))))
        (if (string= (downcase type) "element")
            (setq name (format "%-17s%3s" name "EL"))
          (setq name (format "%-17s%3s" name "ENT")))
        (cons name beg))))

;;; Specialised routine to get the next ada function in the buffer
;;; Michael Polo <mikep@polo.mn.org> <mikep@cfsmo.honeywell.com>
;;;
(defun fume-find-next-ada-function-name (buffer)
  "Searches for the next ada function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward (car fume-function-name-regexp-ada) nil t)
      (let ((beg (match-beginning (cdr fume-function-name-regexp-ada)))
            (end (match-end (cdr fume-function-name-regexp-ada))))

        (if (looking-at fume-function-name-regexp-ada-ignore)
            (fume-find-next-ada-function-name buffer)
          (cons (buffer-substring beg end) beg)))))

;;; Makefiles
;;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
;;;
(defun fume-find-next-function-name-make (buffer)
  "Searches for the next make item in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        (cons (buffer-substring beg end) beg))))

;;; Find next pascal function in the buffer
;;; Espen Skoglund <espensk@stud.cs.uit.no>
;;;
(defun fume-find-next-pascal-function-name (buffer)
  "Searches for the next pascal procedure in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;;; Verilog support
;;; Matt Sale <mdsale@icdc.delcoelect.com>
;;;
(defun fume-find-next-verilog-function-name (buffer)
  "Searches for the next verilog module in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;;; Assembly
;;; Bob Weiner <weiner@mot.com>
;;;
(defun fume-find-next-asm-function-name (buffer)
  "Searches for the next assembler function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (cons (buffer-substring (match-beginning 1) (match-end 1))
            (match-beginning 1))))

;;; This is where you can hook in other languages which may need a different
;;; method to scan for function names. Otherwise, the default defun used is
;;; fume-find-next-function-name which is suitable for sexp-based languages
;;; such as C, C++ and elisp.
;;;
(defconst fume-find-function-name-method-alist
  '((ada-mode        . fume-find-next-ada-function-name)
    (alice-mode      . fume-find-next-python-function-name)
    (asm-mode        . fume-find-next-asm-function-name)
    (bacis-mode      . fume-find-next-bacis-function-name)
    (bibtex-mode     . fume-find-next-bibtex-citation)
    (c++-mode        . fume-match-find-next-function-name)
    (c-mode          . fume-find-next-c-function-name)
    (dired-mode      . fume-find-next-directory-name)
    (ehdm-mode       . fume-find-next-ehdm-entity)
    (fame-mode       . fume-find-next-pascal-function-name)
    (fortran-mode    . fume-find-next-fortran-function-name)
    (ksh-mode        . fume-find-next-ksh-function-name)
    (latex-mode      . fume-find-next-latex-section-name)
    (LaTeX-mode      . fume-find-next-latex-section-name)
    (makefile-mode   . fume-find-next-function-name-make)
    (maple-mode      . fume-find-next-maple-function-name)
    (modula-2-mode   . fume-find-next-modula-function-name)
    (modula-3-mode   . fume-find-next-modula-function-name)
    (pascal-mode     . fume-find-next-pascal-function-name)
    (perl-mode       . fume-find-next-perl-function-name)
    (postscript-mode . fume-find-next-postscript-function-name)
    (prolog-mode .     fume-find-next-prolog-function-name)
    (pvs-mode        . fume-find-next-pvs-entity)
    (python-mode     . fume-find-next-python-function-name)
    (scheme-mode     . fume-find-next-scheme-function)
    (sgml-mode       . fume-find-next-sgml-element-name)
    (tcl-mode        . fume-match-find-next-function-name)
    (verilog-mode    . fume-find-next-verilog-function-name)
    )

  "The connection between a mode and the defun that finds function names.
If no connection is in this alist for a given mode, a default method is used")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;  General utility functions  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Routine to refresh the modeline
;;;
(if (fboundp 'redraw-modeline)          ; faster built-in method
    (defalias 'fume-refresh-modeline 'redraw-modeline)
  (defun fume-refresh-modeline ()       ; use old kludge method
    (set-buffer-modified-p (buffer-modified-p))))

;;; Smart mouse positioning
;;;
(if (fboundp 'window-edges)             ; old method
    (defun fume-set-mouse-position ()
      (set-mouse-position
       (selected-frame)
       (nth 0 (window-edges)) (nth 1 (window-edges))))
  (defun fume-set-mouse-position ()     ; new method
    (set-mouse-position
     (selected-window)
     (nth 0 (window-pixel-edges))
     (nth 1 (window-pixel-edges)))))

;;; Sets 'fume-function-name-regexp' to something appropriate for the current
;;; mode for this buffer.
;;;
(defun fume-set-defaults ()
  "Returns nil if unsuccessful in setting up buffer-local defaults.
Otherwise returns fume-function-name-regexp"
  (setq fume-function-name-regexp
        (symbol-value
         (cdr-safe (assoc major-mode fume-function-name-regexp-alist))))
  (if fume-function-name-regexp
      (setq fume-find-next-function-name-method
            (or (cdr-safe (assoc major-mode
                                 fume-find-function-name-method-alist))
                'fume-find-next-function-name)))
  fume-function-name-regexp)

;;; Routines to add/remove/update function menu from menubar
;;;
(defsubst fume-add-menubar-entry ()
  (interactive)
  (save-window-excursion (function-menu t)))

(defsubst fume-remove-menubar-entry ()
  (interactive)
  (cond ((and fume-running-xemacs current-menubar)
         (delete-menu-item (list fume-menubar-menu-name))
         ;; force update of the menubar
         (fume-refresh-modeline))))

(defsubst fume-update-menubar-entry ()
  "Returns t if menubar was updated. Nil otherwise"
  (and fume-running-xemacs
       fume-not-tty
       (assoc fume-menubar-menu-name current-menubar)
       (fume-add-menubar-entry)
       t))

(defsubst fume-trim-string (string)
  "Returns STRING with leading and trailing whitespace removed."
  (if (string-match "^[ \t]*" (setq string (format "%s" string)))
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t]*$" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)

(defvar fume-syntax-table nil)

(defsubst fume-what-looking-at ()
  (let (name
        (orig-syntax-table (copy-syntax-table (syntax-table))))
    (if fume-syntax-table
        ()
      (setq fume-syntax-table (copy-syntax-table))
      (modify-syntax-entry ?: "w" fume-syntax-table))
    (unwind-protect
        (progn
          (set-syntax-table fume-syntax-table)
          (save-excursion
           (while (looking-at "\\sw\\|\\s_") (forward-char 1))
           (if (re-search-backward "\\sw\\|\\s_" nil t)
               (let ((beg (progn (forward-char 1) (point))))
                 (forward-sexp -1)
                 (while (looking-at "\\s'") (forward-char 1))
                 (setq name (buffer-substring beg (point)))))))
      (set-syntax-table orig-syntax-table)
      name)))

;;; Find function name that point is in.
;;; The trick is to start from the end...
;;;
(defsubst fume-function-before-point ()
  (if (or fume-modeline-funclist (fume-rescan-buffer) fume-modeline-funclist)
      (let (result
            (pt (point)))
        (save-excursion
          (catch 'found
            (mapcar (function
                     (lambda (p)
                       (goto-char (cdr p))
                       (beginning-of-line 1)
                       (if (>= pt (point))
                           (throw 'found (setq result (car p))))))
                    fume-modeline-funclist))
          result))))

;;; Routines to add a buffer local post command hook
;;;
(defsubst fume-post-command-hook-p (hook)
  (memq hook (if fume-use-local-post-command-hook
                 local-post-command-hook
               post-command-hook)))

(defsubst fume-add-post-command-hook (hook &optional append)
  (or (fume-post-command-hook-p hook)
      (if fume-use-local-post-command-hook
          (add-hook 'local-post-command-hook hook append)
        (make-variable-buffer-local 'post-command-hook)
        (add-hook 'post-command-hook hook append))))

(defsubst fume-remove-post-command-hook (hook)
  (if (fume-post-command-hook-p hook)
      (remove-hook
       (if fume-use-local-post-command-hook
           'local-post-command-hook
         'post-command-hook)
       hook)))

;;; Routine to install the modeline feature
;;;
(defsubst fume-maybe-install-modeline-feature ()
  (cond ((and fume-display-in-modeline-p (fume-set-defaults))
         (or fume-modeline-funclist
             (fume-post-command-hook-p 'fume-tickle-modeline)
             (fume-rescan-buffer))
         (fume-add-post-command-hook 'fume-tickle-modeline)
         (fume-remove-post-command-hook 'fume-maybe-install-modeline-feature)
         (fume-tickle-modeline-1)
         (fume-tickle-modeline)
         t                              ; return success flag
         )))

(defun fume-toggle-modeline-display ()
  "Toggles whether func-menu displays function names in the modeline"
  (interactive)
  (setq fume-display-in-modeline-p (not fume-display-in-modeline-p))
  (if (interactive-p) (fume-tickle-modeline)))

;;; Routine to display function before point in the modeline
;;;
(defun fume-tickle-modeline ()
  (let ((fname (and fume-display-in-modeline-p (fume-function-before-point))))
    (set fume-modeline-buffer-identification
         (cond ((and fume-display-in-modeline-p (not (null fname)))
                (setq fname (format "`%s'" (fume-trim-string fname)))
                (if (eq fume-display-in-modeline-p t)
                    (list fume-modeline-buffer-identification-1 " " fname)
                  fname))
               (t
                fume-modeline-buffer-identification-0))))
  (cond ((not fume-display-in-modeline-p)
         (fume-remove-post-command-hook 'fume-tickle-modeline)
         (fume-add-post-command-hook 'fume-maybe-install-modeline-feature)))
  ;; force an update of the mode line
  (fume-refresh-modeline))

(fume-defvar-local fume-modeline-buffer-identification-0 nil
  "Storage for original modeline-buffer-identification")

(fume-defvar-local fume-modeline-buffer-identification-1 nil
  "Storage for munged modeline-buffer-identification")

(defun fume-tickle-f-to-b (str)
  ;; Change modeline format of "XEmacs: %f" to "XEmacs: %b" in order to make
  ;; extra room for the function name which is going to be appended to the
  ;; modeline-buffer-identification component of the modeline-format.
  (cond ((consp str)
	 (mapcar #'fume-tickle-f-to-b str))
	((not (stringp str))
	 str)
	((string-match "%[0-9]*f" str)
	 (let ((newstr (copy-sequence str)))
	   (aset newstr (1- (match-end 0)) (string-to-char "b"))
	   newstr))
	(t str)))

(defun fume-tickle-modeline-1 ()
  (or fume-modeline-buffer-identification-0
      (setq fume-modeline-buffer-identification-0
            (symbol-value fume-modeline-buffer-identification)))
  (setq fume-modeline-buffer-identification-1
        (fume-tickle-f-to-b fume-modeline-buffer-identification-0)))

;;; Routine to create a shallow separate copy of a list
;;;
(if (fboundp 'copy-tree)                ; not built-in in all emacsen
    (defalias 'fume-shallow-copy-list 'copy-tree)
  (defun fume-shallow-copy-list (list)
    (mapcar (function (lambda (i) (cons (car i) (cdr i)))) list)))

;;; Sort function to sort items depending on their function-name
;;; An item looks like (NAME . POSITION).
;;;
(defsubst fume-sort-by-name (item1 item2)
  (or (string-lessp (car item1) (car item2))
      (string-equal (car item1) (car item2))))

;;; Sort function to sort items depending on their position
;;;
(defsubst fume-sort-by-position (item1 item2)
  (<= (cdr item1) (cdr item2)))

;;; Support function to calculate relative position in buffer
;;;
(defsubst fume-relative-position ()
  (let ((pos (point))
        (total (buffer-size)))
    (if (> total 50000)
        ;; Avoid overflow from multiplying by 100!
        (/ (1- pos) (max (/ total 100) 1))
      (/ (* 100 (1- pos))
         (max total 1)))))

;;; Split LIST into sublists of max length N
;;; Example (fume-split '(1 2 3 4 5 6 7 8) 3)-> '((1 2 3) (4 5 6) (7 8))
;;;
(defsubst fume-split (list n)
  (let ((i 0)
        result
        sublist
        (remain list))
    (while remain
      (if (= n (setq sublist (cons (car remain) sublist)
                     remain (cdr remain)
                     i (1+ i)))
          ;; We have finished a sublist
          (setq result (cons (nreverse sublist) result)
                sublist nil
                i 0)))
    ;; There might be a sublist (if the length of LIST mod n is != 0)
    ;; that has to be added to the result list.
    (if sublist
        (setq result (cons (nreverse sublist) result)))
    (nreverse result)))

;;; Routines to create indexes for submenus
;;;

;;; Method 0
;;;
(defun fume-index-sublist-method-0 (sublist count)
  (concat "Function sublist #" count))

;;; Method 1
;;; Thomas Plass <thomas.plass@mid-heidelberg.de>
;;;
(defun fume-index-sublist-method-1 (sublist &rest count)
  (interactive)
  (let ((s (substring (car (car sublist)) 0 1))
        (e (substring (car (nth (1- (length sublist)) sublist)) 0 1)))
    (format "Function sublist (%s%s)"
            s (if (string-equal s e) "<>" (format "<>-%s<>" e)))))

;;; Method 2
;;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
;;;
(defun fume-index-sublist-method-2 (sublist &rest count)
  (let ((s (substring (car (car sublist))
                      0
                      (min (length (car (car sublist))) 12)))
        (e (substring (car (nth (1- (length sublist)) sublist))
                      0
                      (min (length (car (nth (1- (length sublist)) sublist)))
                           12))))
    (format "%s%s" s (if (string-equal s e) "<>" (format "<> ... %s<>" e)))))

;;; Method 3
;;;
(defun fume-index-sublist-method-3-1 (sublist ix limit)
  (let ((s1 (substring (car (car sublist)) 0 (min limit ix)))
        (s2 (substring
             (car (nth (1- (length sublist)) sublist))
             0 (min (length (car (nth (1- (length sublist)) sublist))) ix))))
    (cons s1 s2)))

(defun fume-index-sublist-method-3 (sublist &rest count)
  (let* ((cmplength 12)
         (limit (length (car (car sublist))))
         (result (fume-index-sublist-method-3-1 sublist cmplength limit))
         (str1 (car result))
         (str2 (cdr result)))
    (while (and (string-equal str1 str2) (< cmplength limit))
      (setq cmplength (1+ cmplength)
            result (fume-index-sublist-method-3-1 sublist cmplength limit)
            str1 (car result)
            str2 (cdr result)))
    (cond ((not (string-equal str1 str2))
           (format "%s<> ... %s<>" str1 str2))
          ((< cmplength limit)
           (format "%s<>" str1))
          (t
           (format "%s ..." str1)))))

;;; Buffer rescanning
;;;
(defun fume-rescan-buffer-trigger ()
  "Automatically spots when a buffer rescan becomes necessary"
  (if fume-auto-rescan-buffer-p
      (if (> fume-rescan-trigger-counter 0)
          (setq fume-rescan-trigger-counter (1- fume-rescan-trigger-counter))
        (setq fume-rescan-trigger-counter
              (/ (buffer-size) fume-rescan-trigger-counter-buffer-size))
        (if (or fume-funclist-dirty-p
                (save-excursion
                  (let (find fnam)
                    (condition-case ()
                        (and fume-function-name-regexp
                             (setq fnam (fume-function-before-point))
                             (setq find (symbol-value
                                         'fume-find-next-function-name-method))
                             (progn (end-of-line 1)
                                    (re-search-backward
                                     fume-function-name-regexp nil t))
                             (not (string-equal
                                   fnam
                                   (car (funcall find (current-buffer))))))
                      (error nil)))))
            (let ((fume-scanning-message nil))
              (fume-rescan-buffer))))))

(defsubst fume-install-rescan-buffer-trigger ()
  (cond ((not (fume-post-command-hook-p 'fume-rescan-buffer-trigger))
         (fume-add-post-command-hook 'fume-rescan-buffer-trigger 'append)
         ;; Make narrow-to-region tickle func-menu
         (or (fboundp 'fume-narrow-to-region)
             (fset 'fume-narrow-to-region
                   (symbol-function 'narrow-to-region)))
         (defun narrow-to-region (b e)
           "Restrict editing in this buffer to the current region.
The rest of the text becomes temporarily invisible and untouchable
but is not deleted; if you save the buffer in a file, the invisible
text is included in the file.  C-x n w makes all visible again.
See also `save-restriction'.

When calling from a program, pass two arguments; positions (integers
or markers) bounding the text that should remain visible"
           (interactive "r")
           (fume-narrow-to-region b e)
           (if fume-funclist (setq fume-funclist-dirty-p t)))
         ;; Make widen tickle func-menu
         (or (fboundp 'fume-widen)
             (fset 'fume-widen (symbol-function 'widen)))
         (defun widen ()
           "Remove restrictions (narrowing) from current buffer.
This allows the buffer's full text to be seen and edited."
           (interactive)
           (fume-widen)
           (if fume-funclist (setq fume-funclist-dirty-p t))))))

(defun fume-rescan-buffer (&optional popmenu)
  "Rescans the buffer for function names.
If optional arg POPMENU is non-nil, brings up the function-menu."
  (interactive)
  (let ((find (symbol-value 'fume-find-next-function-name-method))
        (fnam)
        (flst '())
        (buffer-to-scan (current-buffer)))
    (save-excursion
      (goto-char (point-min))
      (cond (fume-scanning-message
             (message fume-scanning-message 0))
            (fume-rescanning-message
             (message fume-rescanning-message)))
      (while (setq fnam
                   (condition-case ()
                       (funcall find buffer-to-scan)
                     (error
                      ;; test for more possible fns after this error trap
                      (save-excursion
                        (re-search-forward
                         fume-function-name-regexp nil t)))))
        (cond ((listp fnam)
               (setq flst (cons fnam flst))
               (if fume-found-function-hook
                   (save-excursion (run-hooks 'fume-found-function-hook)))))
        (if fume-scanning-message
            (message fume-scanning-message (fume-relative-position))))
      (cond (fume-scanning-message
             (message "%s done" (format fume-scanning-message 100)))
            (fume-rescanning-message
             (message "%s done" fume-rescanning-message)))
      ;; make a copy of flst sorted by position in buffer
      (setq fume-modeline-funclist
            (nreverse
             (sort (fume-shallow-copy-list flst) 'fume-sort-by-position)))
      (if fume-sort-function
          (setq fume-funclist (sort flst fume-sort-function))
        (setq fume-funclist (nreverse flst)))
      (if fume-rescan-buffer-hook
          (run-hooks 'fume-rescan-buffer-hook))))
  (if popmenu
      (function-menu)
    (let ((fume-rescan-inhibit-p t))
      (fume-update-menubar-entry)))
  ;; Reset dirty flag
  (setq fume-funclist-dirty-p nil))

;;; Routine to position cursor
;;;
(defun fume-goto-function (fn pos)
  "Position cursor at function FN at location POS"
  (let ((orig-pos (point))
        (case-fold-search nil)
        (match-fn (cond ((string-match "DEFUN " fn) ; Emacs DEFUN declaration
                         (substring fn (match-end 0)))
                        ((string-match "^[ \t]*" fn) ; strip leading spaces
                         (substring fn (match-end 0)))
                        (t
                         fn))))

    (save-excursion
      (goto-char pos)
      (or (looking-at match-fn)
          (let ((fume-scanning-message nil))
            (fume-rescan-buffer)
            (setq pos (cdr-safe (assoc fn fume-funclist))))))

    (if pos
        (progn
          (goto-char pos)
          ;; possibly set mark
          (or (= orig-pos (point))
              (push-mark orig-pos (null fume-scanning-message)))
          (set-window-start
           (selected-window)
           (save-excursion
             (beginning-of-line
              (- 1 (or (and (numberp fume-fn-window-position)
                            (min (- (window-height) 2)
                                 fume-fn-window-position))
                       3)))
             (point))))
      (ding)
      (message "%s not found" fn)
      (function-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;  The main entry points for this package  ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Interface to function-menu for mouse bindings only
;;;
(defun mouse-function-menu (event)
  "Wrapper for mouse button bindings for function-menu"
  (interactive "e")
  (let ((currwin (selected-window)))
    (condition-case ()
        (progn
          (select-window (fume-event-window event))
          (let ((fume-auto-position-popup nil))
            (call-interactively 'function-menu)))
      (error (select-window currwin)))))

;;; Interface for Key bindings
;;;
(defun function-menu (&optional use-menubar)
  "Pop up a menu of functions for selection with the mouse.

With a prefix arg adds the menu to the current menubar.
Jumps to the selected function.  A mark is set at the old position,
so you can easily go back with C-u \\[set-mark-command]."
  (interactive "P")

  (setq use-menubar
        (and use-menubar fume-running-xemacs fume-not-tty current-menubar))

  (catch 'no-functions
    (or (fume-set-defaults)
        (if (not (interactive-p))
            (throw 'no-functions t)
          (error "func-menu does not support the mode \"%s\"" mode-name)))

    ;; Create a list for this buffer only if there isn't any.
    (or fume-funclist
        (if fume-rescan-inhibit-p
            (fume-remove-menubar-entry)
          (fume-rescan-buffer)))
    (or fume-funclist
        (if (not (interactive-p))
            (throw 'no-functions t)
          (error "No functions found in this buffer.")))

    ;; Rescan buffer trigger
    (fume-install-rescan-buffer-trigger)

    ;; Function name in modeline
    (fume-maybe-install-modeline-feature)

    ;; The rest of this routine works only for (Lucid) XEmacs
    (cond (fume-running-xemacs
           ;; Create the menu
           (let* ((count 0)
                  (index-method
                   (intern (format "fume-index-sublist-method-%d"
                                   fume-index-method)))
                  function-menu
                  (function-menu-items
                   (mapcar
                    (function
                     (lambda (sublist)
                       (setq count (1+ count))
                       (cons (format "%s"
                                     (funcall index-method sublist count))
                             (mapcar
                              (function
                               (lambda (menu)
                                 (vector (format "%s" (car menu))
                                         (list 'fume-goto-function
                                               (car menu) (cdr menu))
                                         t)))
                              sublist))))
                    (fume-split fume-funclist fume-max-items))))

             (or (> count 1)
                 (setq function-menu-items (cdr (car function-menu-items))))

             ;;(setq function-menu
             ;;      `([,(concat "Rescan buffer :  " (buffer-name))
             ;;         (fume-rescan-buffer ,(null use-menubar)) t]
             ;;        ["Display full list of functions" fume-list-functions t]
             ;;        ["Toggle modeline display" fume-toggle-modeline-display t]
             ;;        "----"
             ;;        ,@function-menu-items))

             (setq function-menu
                   `(,@function-menu-items
                     "----"
                     [,(concat "Rescan buffer :  " (buffer-name))
                      (fume-rescan-buffer ,(null use-menubar)) t]
                     ["Display full list of functions" fume-list-functions t]
                     ["Toggle modeline display" fume-toggle-modeline-display t]))

             (cond (use-menubar
                    (fume-remove-menubar-entry)
                    (set-buffer-menubar (copy-sequence current-menubar))
                    (fume-add-submenu
                     fume-menubar-menu-name
                     `(,@function-menu
                       "----"
                       ["Remove Function Menu from menubar"
                        fume-remove-menubar-entry t])
                     fume-menubar-menu-location))

                   ((and fume-not-tty   ; trap tty segmentation faults...
                         (not (popup-menu-up-p)))
                    (or (fume-update-menubar-entry)
                        (setq function-menu
                              (cons
                               ["Put Function Menu into menubar"
                                (function-menu t) t]
                               (cons "----" function-menu))))

                    (if fume-auto-position-popup
                        (fume-set-mouse-position))

                    (popup-menu (cons "Functions" function-menu)))))))))

(defun fume-mouse-function-goto (event)
  "Goto function clicked on or prompt in minibuffer (with completion)."
  (interactive "@e")
  (goto-char (event-point event))
  (let ((fume-no-prompt-on-valid-default t))
    (fume-prompt-function-goto)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;  Keyboard access to func-menu for tty users  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Internal variables only
;;;
(defvar fume-list-srcbuffer nil)
(defvar fume-list-reused-win-p nil)
(defvar fume-list-trampled-buffer nil)

;;; Espen Skoglund <espensk@stud.cs.uit.no>
;;; David Hughes <ukchugd@ukpmr.cs.philips.nl>
;;;
(defun fume-prompt-function-goto (&optional other-window-p)
  "Goto function prompted for in minibuffer (with completion).
With prefix arg, jumps to function in a different window."
  (interactive "P")
  (and (interactive-p) current-prefix-arg (setq other-window-p t))
  (let* ((default-name (fume-what-looking-at))
         (OrigBuffer (current-buffer))
         (TargetBuffer
          (if (eq major-mode 'fume-list-mode) fume-list-srcbuffer OrigBuffer))
         (fume-no-prompt-on-valid-default
          (or fume-no-prompt-on-valid-default
              (eq major-mode 'fume-list-mode))))
    (switch-to-buffer TargetBuffer)
    ;; Create funclist and set defaults
    (cond ((null fume-funclist)
           (fume-set-defaults)
           (fume-rescan-buffer)))
    (let* (;; verify default-name is a valid function name
           (default-exists-p (assoc default-name fume-funclist))
           ;; Prompt for function name in minibuffer, unless there is a valid
           ;; function name at point & fume-no-prompt-on-valid-default set to t
           (function-name
            (if (and default-exists-p
                     fume-no-prompt-on-valid-default)
                ""
              (completing-read
               (format "Goto function%s%s: "
                       (if other-window-p " other window" "")
                       (if default-exists-p
                           (concat " (" default-name ")")
                         ""))
               fume-funclist nil t)))
           ;; Use default function name if just RET was pressed
           (function-name (if (and default-exists-p (string= "" function-name))
                              default-name
                            function-name)))
      (switch-to-buffer OrigBuffer)
      ;; Goto function or just return if function name is empty string
      (cond ((not (string= "" function-name))
             (if other-window-p
                 (cond ((prog1 (one-window-p)
                          (switch-to-buffer-other-window TargetBuffer))
                        (other-window 1)
                        (shrink-window-if-larger-than-buffer)
                        (other-window 1)))
               (switch-to-buffer TargetBuffer))
             (fume-goto-function
              function-name (cdr (assoc function-name fume-funclist))))))))

(defun fume-prompt-function-goto-one-window ()
  (interactive)
  (delete-other-windows)
  (fume-prompt-function-goto))

(defun fume-prompt-function-goto-other-window ()
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'fume-prompt-function-goto)))

(defun fume-list-functions-show-other-window ()
  (interactive)
  (select-window
   (prog1 (selected-window)
     (fume-prompt-function-goto-other-window))))

(defun fume-list-functions-help ()
  (interactive)
  (sit-for 0)
  (message " s=%s, q=%s     Goto func: o=%s, g=%s, G=%s"
           "show func"
           "quit"
           "other window"
           "this window"
           "one window"))

(defun fume-list-functions-quit ()
  (interactive)
  (if (eq major-mode 'fume-list-mode)
      (kill-buffer (current-buffer)))
  (if fume-list-reused-win-p
      (condition-case ()
          (switch-to-buffer fume-list-trampled-buffer)
        (error nil))
    (or (one-window-p)
        (delete-window (selected-window))))
  (if (not (eq (current-buffer) fume-list-srcbuffer))
      (condition-case ()
          (select-window (get-buffer-window fume-list-srcbuffer))
        (error
         (condition-case ()
             (switch-to-buffer fume-list-srcbuffer)
           (error nil))))))

(defun fume-list-mouse-select (event)
  (interactive "e")
  (let (ws cb cp (wc (current-window-configuration)))
    (mouse-set-point event)
    (fume-prompt-function-goto-other-window)
    (setq ws (save-excursion
               (beginning-of-line (- 1 fume-fn-window-position)) (point))
          cb (current-buffer)
          cp (point))
    (set-window-configuration wc)
    (switch-to-buffer cb)
    (set-window-start (selected-window) ws)
    (goto-char cp)))

(defvar fume-list-mode-map nil)
(or fume-list-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "q" 'fume-list-functions-quit)
      (define-key map "h" 'fume-list-functions-help)
      (define-key map "?" 'fume-list-functions-help)
      (define-key map "g" 'fume-prompt-function-goto)
      (define-key map "G" 'fume-prompt-function-goto-one-window)
      (define-key map "o" 'fume-prompt-function-goto-other-window)
      (define-key map "s" 'fume-list-functions-show-other-window)
      (if fume-not-tty
          (define-key map [(button2)] 'fume-list-mouse-select))
      (setq fume-list-mode-map map)))

(defvar fume-list-mode-hook nil "*Hook to run after fume-list-mode entered")

(defun fume-list-functions (&optional this-window)
  "Creates a temporary buffer listing functions found in the current buffer"
  (interactive "P")
  (let ((func-near-point (format "^%s$" (fume-function-before-point))))
    (cond ((or fume-function-name-regexp (fume-maybe-install-modeline-feature))
           (save-excursion
             (let ((srcbuffer (current-buffer)))
               (set-buffer (get-buffer-create fume-buffer-name))
               (let (buffer-read-only) (erase-buffer))
               (use-local-map fume-list-mode-map)
               (setq buffer-read-only t
                     mode-name "Func-Menu"
                     major-mode 'fume-list-mode
                     fume-list-srcbuffer srcbuffer
                     fume-list-reused-win-p (not (one-window-p)))
               (if fume-not-tty
                   (setq mode-motion-hook 'mode-motion-highlight-symbol))
               (run-hooks 'fume-list-mode-hook)))
           (or fume-funclist (fume-rescan-buffer))
           (if fume-funclist
               (mapcar (function (lambda (p)
                                   (save-excursion
                                     (set-buffer fume-buffer-name)
                                     (let (buffer-read-only)
                                       (goto-char (point-max))
                                       (insert (concat (car p) "\n"))
                                       (set-buffer-modified-p nil)
                                       (goto-char (point-min))))))
                       fume-funclist))
           (cond ((interactive-p)
                  (if current-prefix-arg
                      (switch-to-buffer fume-buffer-name)
                    (switch-to-buffer-other-window fume-buffer-name)
                    (setq fume-list-trampled-buffer (other-buffer))
                    (or fume-list-reused-win-p
                        (shrink-window-if-larger-than-buffer)))
                  (cond (func-near-point
                         (re-search-forward func-near-point nil t)
                         (beginning-of-line)))
                  (fume-list-functions-help))))

          (t
           (error "Func-Menu is not operative in this buffer")))))

(provide 'func-menu)

