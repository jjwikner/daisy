; eldo-mode.el --- major mode for Eldo netlist files
;;
;; Authors:    Emmanuel Rouat <emmanuel.rouat@wanadoo.fr>
;;             Geert A. M. Van der Plas  <geert_vanderplas@email.com>
;;             Carlin J. Vieri, MIT AI Lab <cvieri@ai.mit.edu> 1994 
;; Maintainer: Emmanuel Rouat <emmanuel.rouat@wanadoo.fr>
;; Version: 1.2.5
;; URL: http://www.esat.kuleuven.ac.be/~vdplas/emacs/
;; Keywords: eldo,spice,analog simulator,netlist
;; Compatibility: XEmacs21, Emacs20 (partly tested)
;;
;; Copyright © 1997-2003 Emmanuel Rouat <emmanuel.rouat@wanadoo.fr>
;; Copyright © 2000-2002 Geert A. M. Van der Plas  <geert_vanderplas@email.com>
;;
;; Eldo is a product of Mentor Graphics(TM)
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMMENTARY:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This major mode for Xemacs-21 provides support for editing ELDO   
;; netlist files. It should also work on Emacs-20 (partially tested)
;;
;; You may wish to add something like the following to your ~/.emacs file:
;; (autoload 'eldo-mode "eldo-mode" nil t)          
;; (setq auto-mode-alist (cons '("\\.cir$"  . eldo-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.ckt$"  . eldo-mode) auto-mode-alist))
;;
;; Put this file in the emacs load-path so emacs can find it (check the manual).
;; (I put it in the dedicated ~/.xemacs/lisp directory)
;; This mode will activate automatically on files which end by .ckt or .cir
;; This mode requires font-lock, easymenu, and optionally func-menu or imenu..
;;
;; Most of the code of this mode was shamelessly stolen from vhdl-mode (ron whitby)
;; and vhdl-electric mode - and from an emacs mode using 'hilit, written at Anacad
;; Also some borrowings from tcl-mode and zillions of .el files
;; Functions/subckt support from spice-mode by C.J Vieri
;; Empty file initialize inspired by rpm-spec-mode (Stig Bjørlykke)
;; (also Changelog support)
;; 'Eldo-colorized-libraries' based on code by Frederic MIENVILLE
;; Some regexps/ideas were taken/shared with Geert Van Der Plas' spice-mode  
;; ( see http://www.esat.kuleuven.ac.be/~vdplas/emacs/ )
;;
;; Good they were there... I HATE ELISP!!!!!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; - more effective templates (use skeleton ?)
;; - indentation engine ?
;; - lots of syntax updates for EldoRF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BUGS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; - The extents used to 'mark' included files/library names screw
;;   up when you edit that name (Xemacs only)
;; - compilation output coloring doesn't work yet - will it ever ?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;------------------------------------------------------------
;; Misc variables/constants
;;------------------------------------------------------------


(defconst eldo-mode-version "1.2.5 February 2003"
  "Current version of eldo mode.")

(defconst eldo-mode-developer   
  "Emmanuel Rouat <emmanuel.rouat@wanadoo.fr> and Geert Van der Plas <geert_vanderplas@email.com>"
  "Current developers/maintainers of eldo-mode.")

(defvar eldo-mode-hook nil
  "Function or functions to run on entry to eldo-mode.")

(defvar eldo-default-header nil
  "Default header for Eldo netlists")

(defvar eldo-end-comment-column (1- fill-column)
  "*Last column in Eldo  comment (before line wraps)")

(defvar eldo-running-xemacs (string-match "XEmacs" emacs-version)
  "A variable that tells us whether we're in Xemacs or not" )

(defvar eldo-tempo-tags nil
  "List of templates used in Eldo mode.")


;;------------------------------------------------------------
;; Customization support
;;------------------------------------------------------------


(defgroup eldo-mode nil
  "Customizations for Eldo Mode."
  :prefix "eldo-mode-"
  :group 'languages)

(defgroup eldo-faces nil
  "Customizations for highlighting."
  :group 'eldo-mode)

(defcustom eldo-use-imenu t
  "*Non-nil means use imenu to create an index menu in the menubar.
This index will list subcircuits, bipolar, mosfet and diode models."
  :type 'boolean
  :group 'eldo-mode)

(defcustom eldo-use-func-menu nil
  "*Non-nil means use func-menu to create an index menu in the menubar.
This index will list subcircuits, bipolar, mosfet and diode models.
Note that func-menu is usually not available in Emacs."
  :type 'boolean
  :group 'eldo-mode)

(defcustom eldo-simulator-switches " -noconf"
  "Eldo command switches, used when simulating buffer with `compile-mode'"
  :type  'string
  :group 'eldo-mode)


(defgroup eldo-section nil
  "Customizations for sections."
  :group 'eldo-mode)

;; sections (entirely different implementation but   
;; sections idea has been taken from eldo-mode.el)
(defcustom eldo-section-alist
  '(
    ;; Libraries
    ("Libraries"     "LIBRARIES"              nil) ;   
    ;; Netlist
    ("Netlist"       "NETLIST"                nil) ;   
    ;; Options	       
    ("Options"       "SIMULATION OPTIONS"     nil) ;
    ;; Supplies	       
    ("Supplies"      "SUPPLIES/REFERENCES"    nil) ;
    ;; Input Signals
    ("Input Signals" "INPUT SIGNALS"          nil) ;
    ;; DC Analysis
    ("DC Analysis"   "DC ANALYSIS"            nil) ;
    ;; AC Analysis
    ("AC Analysis"   "AC ANALYSIS"            nil) ;
    ;; Transient Analysis
    ("Transient Analysis" "TRANSIENT ANALYSIS" nil) ;
    ;;; Add your local spice sections here:
    ;;   
    )
  "*List of valid sections in an Eldo file and their options.
Each list entry specifies the following items for a section:
Section:
  Section Name     : name used in to select/create find section, make this   
                     name short and descriptive.
  Section String   : string used in file to start section (usually all   
                     uppercase variant of name.   
  Extra switches   : extra switches for a section, unspecified for now."
  :type '(repeat (list :tag Section :indent 2
		       (string :tag "Section Name        ")
		       (string :tag "Section String      ")
		       (string :tag "Extra Switches (nil)")))
  :group 'eldo-section)


;;------------------------------------------------------------
;; Required packages
;;------------------------------------------------------------

(require 'font-lock)
(require 'compile)
(require 'tempo)
(require 'imenu)
(require 'speedbar)
(require 'easymenu)
(require 'skeleton)


;;------------------------------------------------------------
;; Key bindings   
;;------------------------------------------------------------


(defvar eldo-mode-map ()
  "Keymap containing eldo commands.")

(if eldo-mode-map   
    ()
  (setq eldo-mode-map (make-sparse-keymap))

  ;; key binding for template completion
  (define-key eldo-mode-map "\t" 'tempo-complete-tag)
  (define-key eldo-mode-map [(shift tab)] 'indent-for-tab-command)
  (define-key eldo-mode-map [(shift iso-lefttab)] 'indent-for-tab-command)

  ;; key bindings (start with Control-C)   
  ;; inspired by vhdl-mode for some compatibility

  ;; key bindings for compile
  (define-key eldo-mode-map "\C-cs"    'eldo-compile)
  (define-key eldo-mode-map "\C-c\C-k" 'kill-compilation)

  (define-key eldo-mode-map "\C-c\C-b" 'eldo-reformat-netlist)
  (define-key eldo-mode-map "\C-c\C-f" 'font-lock-fontify-buffer)

  ;; key bindings for include file loading
  (define-key eldo-mode-map "\C-cl"    'eldo-load-include-files)
  (define-key eldo-mode-map "\C-crl"   'eldo-load-include-files-recursively)

  ;; key bindings for  .subckt search
  (define-key eldo-mode-map "\C-c\C-s"  'eldo-search-subckt)

  ;; key bindings for comment/uncomment 
  (define-key eldo-mode-map "\C-cc"    'comment-region)
  (define-key eldo-mode-map "\C-cu"    'eldo-uncomment-region)
  (define-key eldo-mode-map "\C-c\C-c" 'eldo-comment-uncomment-region)

  ;; key bindings for hiding/unhidding comments 
  (define-key eldo-mode-map "\C-ch"    'eldo-hide-all-comments)
  (define-key eldo-mode-map "\C-c\C-h" 'eldo-unhide-all-comments)

  ;; changelog addition
  (define-key eldo-mode-map "\C-cac"   'eldo-add-changelog-entry)

  ;; mouse bindings
  (define-key eldo-mode-map [(shift mouse-2)] 'eldo-load-file-at-mouse)

  )



;;------------------------------------------------------------
;; Eldo-mode menu (using `easy-menu.el')
;;------------------------------------------------------------

(defun eldo-create-mode-menu ()
  "Create ELDO Mode menu."
  (list
   "Eldo"
    "------"
   '("Edit"
     ["Comment Region"		comment-region (mark)]
     ["Uncomment Region"	eldo-uncomment-region (mark)]
     ["(Un)comment Region"	eldo-comment-uncomment-region (mark)]
     ["Hide Comment Regions"	eldo-hide-all-comments (eldo-hide-all-comments-p)]
     ["Unhide Comment Regions"	eldo-unhide-all-comments eldo-some-comment-regions-are-hidden]
     "--"
     ["Fontify..."		font-lock-fontify-buffer t]
     ["Reformat netlist"	eldo-reformat-netlist t]
     ["Add subckt"		tempo-template-eldo-subckt t]
     ["Add header"		tempo-template-eldo-circuit-header t]
     ["Comment bar"		(eldo-comment-bar 't) t]
     ["Load include/lib files"	eldo-load-include-files  t]     
     ["Same but recursively"	eldo-load-include-files-recursively t]     
     ["Unload all other spice files" eldo-unload-other-decks t]
     "--"
     ["Search .subckt def"	eldo-search-subckt t]
     ["Add Changelog Entry"	eldo-add-changelog-entry t]
     )
    "------"
   '("Simulate"
     ["Run Eldo"		eldo-compile  t]
     ["Quit simulation"		kill-compilation  t]
     )
    "------"
   '("Add Elements"
     ("Passive elements"
      ["Resistor"		tempo-template-eldo-resistor t]
      ["Capacitor"		tempo-template-eldo-capacitor t]
      ["Inductor"		tempo-template-eldo-inductor t]
      ["RC line"		tempo-template-eldo-rcline t]
      )
     ("Active elements"
      ["Diode"			tempo-template-eldo-diode t]
      ["Bipolar"		tempo-template-eldo-bipolar t]
      ["Jfet"			tempo-template-eldo-jfet t]
      ["Mosfet"			tempo-template-eldo-mosfet t]
      )
     ("Voltage Sources"
      ["Voltage controlled"	tempo-template-eldo-vcvs t]
       ["Current controlled"	tempo-template-eldo-ccvs t]
       )
     ("Current Sources"
       ["Voltage controlled"	tempo-template-eldo-vccs t]
       ["Current controlled"	tempo-template-eldo-cccs t]
       )
     ("Waveforms"
       ["PWL"			(eldo-pwl) t]
       ["pulse"			tempo-template-eldo-pulse t]
       ["ac"			tempo-template-eldo-ac t]
       ["am"			tempo-template-eldo-am t]
       ["sin"			tempo-template-eldo-sine t]
       ["sffm"			tempo-template-eldo-sffm t]
       ["exp"			tempo-template-eldo-exp t]
       ["noise"			tempo-template-eldo-noise t]
       ["pattern"		tempo-template-eldo-pattern t]
       )
     ("Macromodels"
      ("Analog"
       ["SO Comparator"		tempo-template-eldo-comp t]
       ["DO Comparator"		tempo-template-eldo-compd t]
       ["SO Linear Opamp"		tempo-template-eldo-linear-opa0 t]
       ["DO Linear Opamp"		tempo-template-eldo-linear-opa0d t]
       ["SO Linear 1-pole Opamp"	tempo-template-eldo-linear-opa1 t]
       ["DO Linear 1-pole Opamp"	tempo-template-eldo-linear-opa1d t]
       ["SO Linear 2-pole Opamp"	tempo-template-eldo-linear-opa2 t]
       ["DO Linear 2-pole Opamp"	tempo-template-eldo-linear-opa2d t]
       ["Delay"			tempo-template-eldo-delay t]
       ["Saturating Resistor"	tempo-template-eldo-satr t]
       ["Voltage Limiter"	tempo-template-eldo-satv t]
       ["Voltage cont. switch"	tempo-template-eldo-vswitch t]
       ["Current cont. switch"	tempo-template-eldo-cswitch t]
       ["Triangular to sine converter"	tempo-template-eldo-tri2sin t]
       ["Staircase generator"	tempo-template-eldo-stairgen t]
       ["Sawtooth generator"	tempo-template-eldo-sawgen t]
       ["Triangle generator"	tempo-template-eldo-trigen t]
       ["Amplitude modulator"	tempo-template-eldo-amm t]
       ["Pulse amplitude modulator"	tempo-template-eldo-pam t]
       ["Sample&Hold"		tempo-template-eldo-saho t]
       ["Track&Hold"		tempo-template-eldo-trho t]
       ["Peak Detector"		tempo-template-eldo-peakd t]
       ["SO Level Detector"	tempo-template-eldo-levdso t]
       ["DO Level Detector"	tempo-template-eldo-levddo t]
       ["Log Amplifier"		tempo-template-eldo-logamp t]
       ["Antilog Amplifier"	tempo-template-eldo-antilog t]
       ["Differentiator"	tempo-template-eldo-diff t]
       ["Integrator"		tempo-template-eldo-integ t]
       ["Add/Sub/Mult/Div"	tempo-template-eldo-adder t]
       )
      ("Digital"
       ["Delay"			tempo-template-eldo-delay t]
       ["Inverter"		tempo-template-eldo-inv t]
       ["XOR gate"		tempo-template-eldo-xor t]
       ["2 input AND gate"	tempo-template-eldo-and2 t]
       ["2 input NAND gate"	tempo-template-eldo-nand2 t]
       ["2 input OR gate"	tempo-template-eldo-or2 t]
       ["2 input NOR gate"	tempo-template-eldo-nor2 t]
       ["3 input AND gate"	tempo-template-eldo-and3 t]
       ["3 input NAND gate"	tempo-template-eldo-nand3 t]
       ["3 input OR gate"	tempo-template-eldo-or3 t]
       ["3 input NOR gate"	tempo-template-eldo-nor3 t]
       )
      ("Mixed Signal"
       ["AD Converter"		tempo-template-eldo-adc t]
       ["DA Converter"		tempo-template-eldo-dac t]
       )
      ("Switched Cap"
       ["Opamp"			tempo-template-eldo-switchcap-opa t]
       ["Switch"		tempo-template-eldo-switch t]
       )
      )
     )
   '("Extracts"
     ["Phase margin"		tempo-template-eldo-phmag t]
     ["PM min"			tempo-template-eldo-pmmin t]
     ["Gain margin"		tempo-template-eldo-gmag t]
     ["Cut freq."		tempo-template-eldo-fc t]
     ["Unity gain freq."	tempo-template-eldo-ugfc t]
     ["Period of signal"	tempo-template-eldo-period t]
     )      
   '("Macros"
     ["Period of signal"	tempo-template-eldo-period-macro t]
     ["Duty cycle of signal"	tempo-template-eldo-duty-macro t]
     ["Settling time of signal"	tempo-template-eldo-settling-macro t]
     ) 
    "--"
    '("ST specific"
     ["MISMATCH=1"		eldo-set-mismatch (mark)]
     ["MISMATCH=0"		eldo-unset-mismatch (mark)]
     )     
    "--"
   (append   
    '("Goto Section")
    (let ((section-alist eldo-section-alist) menu-alist name str)
      (while section-alist
	(setq name (car (car section-alist)))
	(setq str (car (cdr (car section-alist))))
	(setq menu-alist (cons (vector name   
				       (list 'eldo-goto-section str)
				       (list 'eldo-section-p str)
				       )
			       menu-alist))
	(setq section-alist (cdr section-alist)))
      (setq menu-alist   
	    (cons '["Changelog"
		    (eldo-goto-section "Changelog")   
		    (eldo-section-p "Changelog")]
		  (cons "--" menu-alist)))
      (setq menu-alist   
	    (cons '["Specify..."
		    eldo-goto-section t]
		  (cons "--" menu-alist)))
      (nreverse menu-alist))
    )
   (append   
    '("Add Section Header")
    (let ((section-alist eldo-section-alist) menu-alist name str)
      (while section-alist
	(setq name (car (car section-alist)))
	(setq str (car (cdr (car section-alist))))
	(setq menu-alist (cons (vector name   
				       (list 'eldo-add-section str)
				       (list 'not (list 'eldo-section-p str))
				       )
			       menu-alist))
	(setq section-alist (cdr section-alist)))
      (setq menu-alist   
	    (cons '["Changelog"
		    (eldo-add-section "Changelog")   
		    (not (eldo-section-p "Changelog"))]
		  (cons "--" menu-alist)))
      (setq menu-alist   
	    (cons '["Specify..."
		    eldo-add-section t]
		  (cons "--" menu-alist)))
      (nreverse menu-alist))
    )
    "------"
   '("Settings"
     ["Customize..."		eldo-customize t]
     ["Speedbar"		speedbar-frame-mode :style toggle 
      :selected (and (boundp 'speedbar-frame)
		     (frame-live-p speedbar-frame)
		     (frame-visible-p speedbar-frame))]     
     ["Auto fill"		auto-fill-mode :style toggle :selected auto-fill-function]
     )
    "------"
    ["About Eldo-Mode"		eldo-about t])
  )

(defun eldo-update-mode-menu ()
  "Update Eldo mode menu."
  (interactive)
  (easy-menu-remove eldo-menu-list) ; for XEmacs
  (setq eldo-mode-menu-list (eldo-create-mode-menu))
  (easy-menu-add eldo-menu-list)	; for XEmacs
  (easy-menu-define eldo-menu eldo-mode-map
		    "Menu keymap for Eldo Mode." eldo-menu-list))


(defvar eldo-menu-list nil
  "Eldo Mode menu.")

;;------------------------------------------------------------
;; Eldo mode syntax table
;;------------------------------------------------------------


;; In eldo you can also make a block comment, with
;; a #c (begin comment) and #e (end comment)

(defvar eldo-mode-syntax-table nil
  "The syntax table used in `eldo-mode' buffers")

(if eldo-mode-syntax-table
    ()
  (setq eldo-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\n "> b" eldo-mode-syntax-table)
  ;;  (modify-syntax-entry ?! "< b"   eldo-mode-syntax-table)
  (modify-syntax-entry ?# "w 13" eldo-mode-syntax-table)
  (modify-syntax-entry ?c "w 2"  eldo-mode-syntax-table)
  (modify-syntax-entry ?e "w 4"  eldo-mode-syntax-table)
  (modify-syntax-entry ?/ "w"    eldo-mode-syntax-table)
  (modify-syntax-entry ?' "w"    eldo-mode-syntax-table)
  (modify-syntax-entry ?. "w"    eldo-mode-syntax-table)
  (modify-syntax-entry ?, "w"    eldo-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    eldo-mode-syntax-table)
  (modify-syntax-entry ?- "w"    eldo-mode-syntax-table)
  (modify-syntax-entry ?@ "w"    eldo-mode-syntax-table)
  (modify-syntax-entry ?[ "("    eldo-mode-syntax-table)
  (modify-syntax-entry ?] ")"    eldo-mode-syntax-table)
  (modify-syntax-entry ?= "."    eldo-mode-syntax-table)
  (set-syntax-table eldo-mode-syntax-table))

;; note: in eldo syntax, nearly all caracters are valid words! For instance
;; a model can be called BIP_TYPICAL#1.2[foo]
;; this is why nearly all caracters here have word-class




;;------------------------------------------------------------
;; font-lock stuff
;;------------------------------------------------------------


;; We try to use usual font-lock faces, plus a few specific ones:


(custom-add-to-group
 'eldo-faces 'font-lock-comment-face 'custom-face)
(custom-add-to-group
 'eldo-faces 'font-lock-keyword-face 'custom-face)
(custom-add-to-group
 'eldo-faces 'font-lock-type-face 'custom-face)
(custom-add-to-group
 'eldo-faces 'font-lock-function-name-face 'custom-face)
(custom-add-to-group
 'eldo-faces 'font-lock-variable-name-face 'custom-face)
(custom-add-to-group
 'eldo-faces 'font-lock-warning-face 'custom-face)



(defvar eldo-title-face	'eldo-title-face
  "Face name to  highlight eldo netlist title lines.")

(defface eldo-title-face
  '((((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Red" :bold t))
    (t (:inverse-video t)))
  "Face name to highlight eldo netlist title lines."
  :group 'eldo-faces)

(defvar eldo-doc-face	'eldo-doc-face
  "Face name to highlight eldo netlist documentation.")

(defface eldo-doc-face
  '((((class color) (background light)) (:foreground "blue1"))
    (((class color) (background dark)) (:foreground "blue1"))
    (t (:inverse-video t)))
  "Face name to highlight eldo netlist documentation."
  :group 'eldo-faces)


(defvar eldo-analysis-face 'eldo-analysis-face
  "Face name to highlight eldo analysis commands.")

(defface eldo-analysis-face
  '((((class color) (background light)) (:foreground "Red2" :bold t))
    (((class color) (background dark)) (:foreground "Lightgreen" :bold t))
    (t (:bold t)))
  "Eldo mode face used to highlight analysis commands."
  :group 'eldo-faces)

(defvar eldo-output-face 'eldo-output-face
  "Face name to highlight eldo output commands.")

(defface eldo-output-face
  '((((class color) (background light)) (:foreground "purple2"))
    (((class color) (background dark)) (:foreground "magenta2")))
  "Eldo mode face used to highlight output commands."
  :group 'eldo-faces)

(defvar eldo-instance-name-face 'eldo-instance-name-face
  "Face name to highlight eldo instance names.")

(defface eldo-instance-name-face
  '((((class color) (background light)) (:foreground "Forestgreen" :bold t))
    (((class color) (background dark)) (:foreground "Yellow":bold t))
    (t (:bold t)))
  "Eldo mode face used to highlight instance names."
  :group 'eldo-faces)

(defvar eldo-model-name-face 'eldo-model-name-face
  "Face name to highlight model names in instanciations.")

(defface eldo-model-name-face
  '((((class color) (background light)) (:foreground "Red3"))
    (((class color) (background dark)) (:foreground "Red3"))
    (t (:bold t)))
  "Eldo mode face used to highlight model names in instanciations."
  :group 'eldo-faces)


;; ELDO is case-insensitive
(put 'eldo-mode 'font-lock-keywords-case-fold-search t)



;;------------------------------------------------------------
;; List of Eldo keywords/syntax - order is important!!
;;------------------------------------------------------------

(defvar eldo-doc-starter "\\(^\\|\\s-\\)!"
  "Regexp that describes the start of a doc comment")


(defconst eldo-model-name "\\([a-z]\\sw+[^ \t\n=]*\\)"
  "Regexp that describes a syntactically correct model or subckt name")

(defconst eldo-instance-name "\\([xmdq][^ \t\n]+\\)"
  "Regexp that describes a syntactically correct subckt/mosfet/diode/bipolar instance name")

(defconst eldo-line-break "\\(\n\\s-*\\+\\s-*\\)*"
  "Regexp that matches a (possible) line break (\n+)")

(defconst eldo-library-regexp   
  "^\\s-*[!*]*\\.\\(include\\|lib\\|libfas\\)\\s-+\\(?:key=\\w+\\s-\\)*"
  "Regexp that matches the beginning of library filename")

(defconst eldo-xinstance-regexp
    (concat 
     "^\\s-*" eldo-instance-name
     "\\(\\([ \t]+[^ *!:(=\t\n][^ :(=\t\n]*\\|[ \t]*\\(\n?[*!].*\\)?\n[+]\\)*\\s-*\\)"
;     "\\(\\s-+[^!(=\t\n]+\\|\n\\s-*[+]\\)*\\s-*"
     "\\<" eldo-model-name "\\>"
     "\\(\\s-*\n\\|\\s-+[^=\n]\\)" )
  "Regexp for x instances.")

(defconst eldo-analysis-keywords
  '(
    "ac" "checksoa" "conso" "dc" "four" "mc" "mprun"
    "noise" "noisetran" "op" "pz" "sens" "snf" "sst" "sstnoise"
    "step" "temp" "tf" "tran"  "wcase" 
    )
  "List of Eldo analysis type keywords")

(defconst eldo-model-keywords
  '(
    "res" "r" "cap" "ind"   
    "npn" "pnp" "lpnp" "d"
    "nmos" "pmos" "njf" "pjf" "nsw" "psw"
    "opa" "modfas" "logic" "a2d" "d2a"
    "macro"
    )
  "List of Eldo models")

(defconst eldo-macromodel-keywords
  '(
    "opamp0" "opamp0d" "opamp1" "opamp1d" "opamp2" "opamp2d" 
    "satr" "satv" "vswitch" "cswitch"
    "tri2sin" "stairgen" "sawgen" "trigen" 
    "amm" "pam" 
    "sa_ho" "tr_ho" 
    "pwm" "vco" 
    "peak_d" "lev_d"
    "logamp" "expamp" 
    "diff" "integ"
    "add" "sub" "mult" "div"
    "sc_ideal" "sc_i" "sc_n" "sc_p" "sc_s1" "sc_s2" 
    "sc_sp1" "sc_sp2" "sc_b" "sc_u" 
    )
  "List of Eldo macromodels")

(defconst eldo-commands
  '(
    "a2d" "addlib" "alter" 
    "checkbus" "chrand" "chrent" 
    "chrsim" "comchar" "connect"
    "d2a" "data" "defmac" "defplotdig" "defwave"   
    "del" "distrib" "dspf_include" 
    "end" "ends" "enddata" "endl" "extract" 
    "ffile"
    "global" "guess" "hier" 
    "ic" "include" "init" 
    "lib" "libfas" "load" "loop" "lotgroup"  
    "mcmod" "moddup" "modlogic" "meas"  
    "newpage" "nocom" "nodeset" "notrc" 
    "optfour" "option" "options"   
    "optnoise" "optpwl" "optwind" 
    "param" "plotbus" "probe" "protect" 
    "ramp" "restart"   
    "save" "setbus" "setsoa" "sigbus"   
    "sinus" "solve" "subdup" 
    "sstprobe"
    "table" "title" "topcell" "tvinclude"  
    "use" "unprotect" 
    "width"   
    )
  "List of Eldo commands (dot cards)")

(defconst eldo-keywords
  '(
    "ac" "dc" "exp" "guess" "ic"
    "list" "nodeset" "nonoise" "overwrite_input"
    "param" "pattern" "pulse" "pwl"   
    "sin" "sweep" "table"
    "port" "generic"	;;vhdl-ams stuff
    )
  "List of Eldo reserved keywords (that are not commands)")

(defvar  eldo-font-lock-keywords
  (list
   ;; Title line
   '("\\`.+$" 0 eldo-title-face)

   ;; comments - lines starting with '*' are ignored by eldo
   '("^\\s-*\\*[^!\n]*" 0 font-lock-comment-face t)

   ;; doc comments - used to document your netlist  
;;   '("![^*\n].*" 0 eldo-doc-face t)
   (list   
    (concat eldo-doc-starter "[^*\n].*")
     '(0 eldo-doc-face t))

   ;; analysis - keywords like .op, .tran etc (eldo-analysis-face)
   (list   
    (concat
     "^\\s-*\\.\\("
     (regexp-opt eldo-analysis-keywords)
     "\\)\\>")
     '(0 eldo-analysis-face))
     
   ;; print,plot,extract,meas,conso
   (list   
    (concat
     "^\\s-*\\.\\(print\\|plot\\|extract\\|meas\\)\\s-*" eldo-line-break
     (regexp-opt '( "ac" "dc" "dcac" "dcsweep" "tran" "dctran" "noise"
		    "four" "sweep" 
		    "fsst" "tsst" "sstnoise" 
		    ) t )
     "\\>" )
     '(0  eldo-output-face))
	   
   ;; plotbus , probe , newpage
   '("^[ \t]*\\.\\(plotbus\\|probe\\|newpage\\)\\>" . eldo-output-face)
	   
   ;; subckt and ends statement
   (list   
    (concat
     "^\\s-*"
     "\\.\\(subckt\\|ends\\)"
     "\\s-+" eldo-model-name "\\>")
    '(1 font-lock-keyword-face)
    '(2 font-lock-function-name-face nil t))

   ;; model definition
   (list   
    (concat
     "^\\s-*"
     "\\(\\.model\\)"
     "\\s-+" eldo-model-name eldo-line-break "\\s-+\\("
     (regexp-opt eldo-model-keywords)
     "\\)\\>" )
    '(1 font-lock-keyword-face)
    '(2 font-lock-function-name-face)
    '(4 font-lock-type-face))

	   
   ;; assignments (xx=zz) - slows down fontification a lot
   '("\\(\\<\\w*\\(\\s(\\w*\\s)\\)*\\)\\s-*=" 1 font-lock-variable-name-face)
   ;; slightly faster
   ;;'("\\<\\([^ \t\n]+\\)\\s-*=" 1 font-lock-variable-name-face)
	    
   ;; libraries, includes....
   (list
    (concat   
     eldo-library-regexp
     "\\s-*\\<\\(\\w+\\)\\>" )
    '(1 font-lock-keyword-face)   
    '(2 eldo-model-name-face nil t))

   ;; 'Y' instances (macromodels)
   (list   
    (concat
     "^\\s-*\\(Y[^ \t\n]+\\)" eldo-line-break "\\s-+\\("
     (regexp-opt eldo-macromodel-keywords)
     "\\)\\>" )
    '(1 eldo-instance-name-face)
    '(3 eldo-model-name-face))

   ;; instances of subcircuits/mosfets/diodes
   (list
    eldo-xinstance-regexp
    '(1 eldo-instance-name-face)   
    '(5 eldo-model-name-face))
	    
   ;; some keywords
   (list
    (concat
     "\\<\\("
     (regexp-opt eldo-keywords )
     "\\)\\>" )
    '(0 font-lock-keyword-face))


   ;; '.' keywords (commands)
   (list   
    (concat
     "^\\s-*\\.\\("
     (regexp-opt eldo-commands)
     "\\)\\>")
    '(0 font-lock-keyword-face))
	   
   ;; highlight additional . unknowns (to avoid stupid typing errors)
   '("^[ \t]*[\.][^ \t\n]*" 0 font-lock-warning-face)
	   
   ;; other keywords - every non fontified word that starts a line    
   ;; except '+' which is line continuation, '!' that starts a doc comment
   ;; and '.' that starts a command.
   '("^[ \t]*[^+!\n.]\\w+" . eldo-instance-name-face)

   )
  "Regexp describing fontification syntax of Eldo keywords"
)



;;------------------------------------------------------------
;; Misc formating functions
;;------------------------------------------------------------


(defun eldo-uncomment-region (beg end)
  "Uncomment selected region - comment symbol is '*'"
  (interactive "r")
  (comment-region beg end -1))


(defun eldo-comment-uncomment-region (beg end &optional arg)
  "Comment out line in region if not commented out, uncomment it if already
commented out (exclusive-or type of comment) - comment symbol is '*' "
  (interactive "r\nP")
  (goto-char beg)
  (narrow-to-region beg end)
  (while (not(eobp))
    (beginning-of-line)
    (if (looking-at comment-start)
	(delete-char (length comment-start))
      (if (looking-at "[ \t]*$") ()
	(insert comment-start) ) )
    (forward-line 1)
    )
  (widen))



(defun eldo-comment-bar (&optional aligned)
  "Insert solid comment bar from column zero to end of line. If optional
   argument is provided, bar will be added from current column."
  (interactive)
  (if (not aligned) (beginning-of-line) )
  (insert "!")
  (insert-char ?- (- eldo-end-comment-column (current-column)))
  (insert "\n"))


(defun eldo-about ()
  (interactive)
  (sit-for 0)
  (message "Eldo-mode version %s, © %s" eldo-mode-version eldo-mode-developer))

;; the next function could be much more complete
(defun eldo-reformat-netlist ()
  (interactive)
  (let ((pos (point)))
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*\\+\\([^ ]\\)" nil t)
      (replace-match "+ \\1" ))
    (font-lock-fontify-buffer)
    (goto-char pos))
  )

(defun eldo-set-mismatch (beg end)
  "Set MISMATCH parameter ON for devices in selected region"
  (interactive "r")
  (goto-char beg)
  (narrow-to-region beg end)
    (while (search-forward "MISMATCH=0" nil t)
      (replace-match "MISMATCH=1" nil t ))  
    (widen))

(defun eldo-unset-mismatch (beg end)
  "Set MISMATCH parameter OFF for devices in selected region"
  (interactive "r")
  (goto-char beg)
  (narrow-to-region beg end)
    (while (search-forward "MISMATCH=1" nil t)
      (replace-match "MISMATCH=0" nil t ))  
    (widen))

(defun eldo-customize ()
  "Call the customize function with `eldo-mode' as argument."
  (interactive)
  (customize-browse 'eldo-mode))


;;------------------------------------------------------------
;; Templates/skeletons
;;------------------------------------------------------------

;; -------------------
;; Passive elements
;; -------------------

(tempo-define-template
 "eldo-resistor"
 '("R"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<mname>: " mname) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'mname) "") 
       (list 'l "r=" '(p "[val]: "))
     (list 'l '(p "[val]: "))) '(just-one-space)
   (p "<temp coef 1>: " tc1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tc1) "") () 
     (list 'l "TC1=" '(s tc1) '(p "<temp coef 2>: " tc2 'noinsert))) 
   '(just-one-space)
   (if (and (tempo-lookup-named 'tc2) 
	    (not (string-equal (tempo-lookup-named 'tc2) ""))) 
       (list 'l "TC2=" '(s tc2))) 
   '(just-one-space)
   (p "<ac resistance>: " ac 'noinsert) 
   (if (string-equal (tempo-lookup-named 'ac) "") 
       () (list 'l "AC=" '(s ac))) 
   '(just-one-space)
   (p "<temp>: " temp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'temp) "") 
       () (list 'l "T=" '(s temp)))
   '(just-one-space)
   (p "<m>: " m 'noinsert) 
   (if (string-equal (tempo-lookup-named 'm) "") 
       () (list 'l "M=" '(s m))) 
   '(just-one-space)
   (p "<nonoise in transient [y/n]?>: " nonoise 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nonoise) "y") 
       (list 'l "NONOISE")) 
   '(just-one-space)
   (p "<kf>: " kf 'noinsert) 
   (if (string-equal (tempo-lookup-named 'kf) "") 
       () (list 'l '(s kf) '(p "<af>: " af 'noinsert))) 
   '(just-one-space)
   (if (and (tempo-lookup-named 'af)
	    (not (string-equal (tempo-lookup-named 'af) ""))) 
       (list 'l '(s af))) 
   '(just-one-space)
   )
 "R"
 "tempo template for eldo resistor"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-capacitor"
 '("C"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<mname | POLY>: " mname) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'mname) "POLY") 
       (list 'l '(p "[val and poly coefficients]: "))
     (list 'l '(p "[val]: ")))
   '(just-one-space)
   (p "<m>: " m 'noinsert) 
   (if (string-equal (tempo-lookup-named 'm) "") 
       () (list 'l "M=" '(s m))) 
   '(just-one-space)
   (p "<length>: " l 'noinsert) 
   (if (string-equal (tempo-lookup-named 'l) "") 
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert) 
   (if (string-equal (tempo-lookup-named 'w) "") 
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'dtemp) "") 
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   (p "<temp coef 1>: " tc1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tc1) "") () 
     (list 'l "TC1=" '(s tc1) '(p "<temp coef 2>: " tc2 'noinsert))) 
   '(just-one-space)
   (if (and (tempo-lookup-named 'tc2) 
	    (not (string-equal (tempo-lookup-named 'tc2) ""))) 
       (list 'l "TC2=" '(s tc2))) 
   '(just-one-space)
   (p "<initial cond (voltage)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "") 
       () (list 'l "IC=" '(s ic)))
   )
 "C"
 "tempo template for eldo capacitor"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-inductor"
 '("L"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<mname | POLY>: " mname) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'mname) "POLY") 
       (list 'l '(p "[val and poly coefficients]: "))
     (list 'l '(p "[val]: ")))
   '(just-one-space)
   (p "<initial cond (current)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "") 
       () (list 'l "ic=" '(s ic)))
   )
 "L"
 "tempo template for eldo inductor"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-rcline"
 '("R"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "[mname]: " mname) '(just-one-space)
   (p "<res>: " r 'noinsert) 
   (if (string-equal (tempo-lookup-named 'r) "") 
       () (list 'l "R=" '(s r)))
   '(just-one-space)
   (p "<temp coef 1>: " tc1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tc1) "") () 
     (list 'l "TC1=" '(s tc1) '(p "<temp coef 2>: " tc2 'noinsert))) 
   '(just-one-space)
   (if (and (tempo-lookup-named 'tc2) 
	    (not (string-equal (tempo-lookup-named 'tc2) ""))) 
       (list 'l "TC2=" '(s tc2))) 
   '(just-one-space)
   (p "<cap>: " c 'noinsert) 
   (if (string-equal (tempo-lookup-named 'c) "") 
       () (list 'l "C=" '(s c)))
   '(just-one-space)
   (p "<length>: " l 'noinsert) 
   (if (string-equal (tempo-lookup-named 'l) "") 
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert) 
   (if (string-equal (tempo-lookup-named 'w) "") 
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<m>: " m 'noinsert) 
   (if (string-equal (tempo-lookup-named 'm) "") 
       () (list 'l "M=" '(s m))) 
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'dtemp) "") 
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   (p "<scale>: " scale 'noinsert) 
   (if (string-equal (tempo-lookup-named 'scale) "") 
       () (list 'l "SCALE=" '(s scale))) 
   '(just-one-space)
   )
 "RC"
 "tempo template for eldo rcline"
 'eldo-tempo-tags
)

;; -------------------
;; Active elements
;; -------------------

(tempo-define-template
 "eldo-diode"
 '("D"
   (p "[name]: ") '(just-one-space)  
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: " area 'noinsert) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'area) "") 
       () (list 'l "AREA=" '(s area)))
   '(just-one-space)
   (p "<perimeter>: " peri 'noinsert)
   (if (string-equal (tempo-lookup-named 'peri) "") 
       () (list 'l "PERI=" '(s peri)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'temp) "") 
       () (list 'l "TEMP=" '(s temp)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF")) 
   '(just-one-space)
   (p "<nonoise [y/n]>: " nonoise 'noinsert)
   (if (and (tempo-lookup-named 'nonoise)
	    (string-equal (tempo-lookup-named 'nonoise) "y"))
       (list 'l "NONOISE")) 
   '(just-one-space)
   )
 "D"
 "tempo template for eldo diode"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-bipolar"
 '("Q"
   (p "[name]: ") '(just-one-space)  
   (p "[collector node]: ") '(just-one-space)
   (p "[base node]: ") '(just-one-space)
   (p "[emitter node]: ") '(just-one-space)
   (p "<substrate node>: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: " area 'noinsert) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'area) "") 
       () (list 'l "AREA=" '(s area)))
   '(just-one-space)
   (p "<rel base area>: " areab 'noinsert)
   (if (string-equal (tempo-lookup-named 'areab) "") 
       () (list 'l "AREAB=" '(s areab)))
   '(just-one-space)
   (p "<rel collector area>: " areac 'noinsert)
   (if (string-equal (tempo-lookup-named 'areac) "") 
       () (list 'l "AREAC=" '(s areac)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'temp) "") 
       () (list 'l "T=" '(s temp)))
   '(just-one-space)
   (p "<m>: " m 'noinsert) 
   (if (string-equal (tempo-lookup-named 'm) "") 
       () (list 'l "M=" '(s m))) 
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF")) 
   '(just-one-space)
   (p "<nonoise [y/n]>: " nonoise 'noinsert)
   (if (and (tempo-lookup-named 'nonoise)
	    (string-equal (tempo-lookup-named 'nonoise) "y"))
       (list 'l "NONOISE")) 
   '(just-one-space)
   )
 "Q"
 "tempo template for eldo bipolar"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-jfet"
 '("J"
   (p "[name]: ") '(just-one-space)  
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: " area 'noinsert) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'area) "") 
       () (list 'l "AREA=" '(s area)))
   '(just-one-space)
   (p "<length>: " l 'noinsert) 
   (if (string-equal (tempo-lookup-named 'l) "") 
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert) 
   (if (string-equal (tempo-lookup-named 'w) "") 
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'temp) "") 
       () (list 'l "T=" '(s temp)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF")) 
   '(just-one-space)
   (p "<nonoise [y/n]>: " nonoise 'noinsert)
   (if (and (tempo-lookup-named 'nonoise)
	    (string-equal (tempo-lookup-named 'nonoise) "y"))
       (list 'l "NONOISE")) 
   '(just-one-space)
   )
 "J"
 "tempo template for eldo jfet"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-mosfet"
 '("M"
   (p "[name]: ") '(just-one-space)  
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[bulk node]: ") '(just-one-space)
   (p "<optional nodes>: ") '(just-one-space)
   "MOD="
   (p "[mname]: ") '(just-one-space)
   (p "<length>: " l 'noinsert) 
   (if (string-equal (tempo-lookup-named 'l) "") 
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert) 
   (if (string-equal (tempo-lookup-named 'w) "") 
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<area drain>: " ad 'noinsert) 
   (if (string-equal (tempo-lookup-named 'ad) "") 
       () (list 'l "AD=" '(s ad)))
   '(just-one-space)
   (p "<area source>: " as 'noinsert) 
   (if (string-equal (tempo-lookup-named 'as) "") 
       () (list 'l "AS=" '(s as)))
   '(just-one-space)
   (p "<perimeter drain>: " pd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'pd) "") 
       () (list 'l "PD=" '(s pd)))
   '(just-one-space)
   (p "<perimeter source>: " ps 'noinsert) 
   (if (string-equal (tempo-lookup-named 'ps) "") 
       () (list 'l "PS=" '(s ps)))
   '(just-one-space)
   (p "<geometry model>: " geo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'geo) "") 
       () (list 'l "GEO=" '(s geo)))
   '(just-one-space)
   (p "<number of drain contacts>: " nrd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nrd) "") 
       () (list 'l "NRD=" '(s nrd)))
   '(just-one-space)
   (p "<number of source contacts>: " nrs 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nrs) "") 
       () (list 'l "NRS=" '(s nrs)))
   '(just-one-space)
   (p "<m>: " m 'noinsert) 
   (if (string-equal (tempo-lookup-named 'm) "") 
       () (list 'l "M=" '(s m))) 
   '(just-one-space)
   (p "<extra drain contact resistance>: " rdc 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rdc) "") 
       () (list 'l "RDC=" '(s rdc)))
   '(just-one-space)
   (p "<extra source contact resistance>: " rsc 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rsc) "") 
       () (list 'l "RSC=" '(s rsc)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'temp) "") 
       () (list 'l "T=" '(s temp)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF")) 
   '(just-one-space)
   (p "<nonoise [y/n]>: " nonoise 'noinsert)
   (if (and (tempo-lookup-named 'nonoise)
	    (string-equal (tempo-lookup-named 'nonoise) "y"))
       (list 'l "NONOISE")) 
   '(just-one-space)
   )
 "M"
 "tempo template for eldo mosfet"
 'eldo-tempo-tags
)

;; -------------------
;; Voltage sources
;; -------------------

(tempo-define-template
 "eldo-vcvs"
 '("E"
   (p "[name]: ") '(just-one-space)    
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[positive controlling node]: ") '(just-one-space)
   (p "[negative controlling node]: ") '(just-one-space)
   (p "[gain]: ") '(just-one-space)
   )   
 "vcvs"
 "template for inserting an ELDO voltage controlled voltage source"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-ccvs"
 '("H"
   (p "[name]: ") '(just-one-space)    
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[current source]: ") '(just-one-space)
   (p "[gain]: ") '(just-one-space)
   )   
 "ccvs"
 "template for inserting an ELDO current controlled voltage source"
 'eldo-tempo-tags
)


;; -------------------
;; Current sources
;; -------------------

(tempo-define-template
 "eldo-vccs"
 '("G"
   (p "[name]: ") '(just-one-space)    
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[positive controlling node]: ") '(just-one-space)
   (p "[negative controlling node]: ") '(just-one-space)
   (p "[transadmitance]: ") '(just-one-space)
   )   
 "vccs"
 "template for inserting an ELDO voltage controlled current source"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-cccs"
 '("F"
   (p "[name]: ") '(just-one-space)    
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[current source]: ") '(just-one-space)
   (p "[gain]: ") '(just-one-space)
   )   
 "cccs"
 "template for inserting an ELDO current controlled current source"
 'eldo-tempo-tags
)


;; -------------------
;; Waveforms
;; -------------------

(define-skeleton eldo-pwl
  "Skeleton for Piece Wise Linear waveform"
  "Time/value doublet: "
  "pwl(" str   
  ( "Next doublet: (%s) "   
    " "str )
  resume:
  ")")

(tempo-define-template
 "eldo-pulse"
 '("Pulse("
   (p "[start value]: ") '(just-one-space)    
   (p "[pulsed value]: ") '(just-one-space)
   (p "[delay]: ") '(just-one-space)   
   (p "[rise time]: ") '(just-one-space)
   (p "[fall time]: ") '(just-one-space)   
   (p "[pulse duration]: ") '(just-one-space)   
   (p "[period]: ")
   ")")   
 "pulse"
 "template for inserting an ELDO Pulse waveform"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-ac"
 '("ac("
   (p "[magnitude]: ") '(just-one-space)    
   (p "[phase]: ") '(just-one-space)
   ")")   
 "ac"
 "template for inserting an ELDO AC waveform"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-am"
 '("am("
   (p "[amplitude]: ") '(just-one-space)    
   (p "[offset]: ") '(just-one-space)
   (p "[modulation frequency]: ") '(just-one-space)
   (p "[carrier frequency]: ") '(just-one-space)
   (p "[delay]: ") '(just-one-space)
   ")")   
 "am"
 "template for inserting an ELDO amplitude modulation signal"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-pattern"
 '("Pattern "
   (p "[Vhi]: ") '(just-one-space)    
   (p "[Vlo]: ") '(just-one-space)
   (p "[delay]: ") '(just-one-space)   
   (p "[rise time]: ") '(just-one-space)
   (p "[fall time]: ") '(just-one-space)   
   (p "[Bit duration]: ") '(just-one-space)   
   (p "[Bits]: ")
   )   
 "pattern"
 "template for inserting an ELDO Pattern function"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-sine"
 '("sin("
   (p "[Offset]: ") '(just-one-space)    
   (p "[Amplitude]: ") '(just-one-space)
   (p "[Frequency]: ") '(just-one-space)   
   (p "[Delay]: ") '(just-one-space)   
   (p "[Damping factor]: ") '(just-one-space)   
   (p "[Phase delay]: ") '(just-one-space)   
   ")")   
 "sin"
 "template for inserting an ELDO SINE function"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-sffm"
 '("sffm("
   (p "[Offset]: ") '(just-one-space)    
   (p "[Amplitude]: ") '(just-one-space)
   (p "[Carrier frequency]: ") '(just-one-space)   
   (p "[Modulation index]: ") '(just-one-space)   
   (p "[Signal frequency]: ") '(just-one-space)   
   ")")   
 "sffm"
 "template for inserting an ELDO Single Frequency FM function"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-exp"
 '("exp("
   (p "[start value]: ") '(just-one-space)    
   (p "[target value]: ") '(just-one-space)
   (p "[rise delay]: ") '(just-one-space)
   (p "[tau1]: ") '(just-one-space)
   (p "[fall delay]: ") '(just-one-space)
   (p "[tau2]: ") '(just-one-space)
   ")")   
 "exp"
 "template for inserting an ELDO EXP waveform"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-noise"
 '("noise("
   (p "[White noise level]: ") '(just-one-space)    
   (p "[Flicker noise level]: ") '(just-one-space)
   (p "[Alpha]: ") '(just-one-space)
   (p "[Cut-off freq]: ") '(just-one-space)
   (p "[Filter order]: ") '(just-one-space)
   ")")   
 "noise"
 "template for inserting an ELDO NOISE waveform"
 'eldo-tempo-tags
)

;; -------------------
;; Analog Macromodels
;; -------------------

(tempo-define-template
 "eldo-comp"
 '("COMP"
   (p "[Instance name]: ") " "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value 
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Hysteresis voltage>: " vdef 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vdef) "") 
       (list 'l "VDEF=0.0")	;; default value 
     (list 'l "VDEF=" '(s vdef)))
   '(just-one-space)
   (p "<Commutation time>: " tcom 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tcom) "") 
       (list 'l "TCOM=1ns")	;; default value 
     (list 'l "TCOM=" '(s tcom)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=0.0")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   'n)   
 "comp"
 "template for inserting an ELDO Single output comparator"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-compd"
 '("COMPD"
   (p "[Instance name]: ") " "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value 
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Hysteresis voltage>: " vdef 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vdef) "") 
       (list 'l "VDEF=0.0")	;; default value 
     (list 'l "VDEF=" '(s vdef)))
   '(just-one-space)
   (p "<Commutation time>: " tcom 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tcom) "") 
       (list 'l "TCOM=1ns")	;; default value 
     (list 'l "TCOM=" '(s tcom)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=0.0")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   'n)   
 "compd"
 "template for inserting an ELDO Differential output comparator"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-linear-opa0"
 '("Y"
   (p "[Instance name]: ") " OPAMP0 " 
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert) 
   (if (string-equal (tempo-lookup-named 'gain) "") 
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rin) "") 
       (list 'l "RIN=1e7") 	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   'n)
 "opa0"
 "tempo template for eldo single output linear opamp"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-linear-opa0d"
 '("Y"
   (p "[Instance name]: ") " OPAMP0D "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert) 
   (if (string-equal (tempo-lookup-named 'gain) "") 
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rin) "") 
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   'n)   
 "opa0d"
 "template for inserting an ELDO differential output linear opamp"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-linear-opa1"
 '("Y"
   (p "[Instance name]: ") " OPAMP1 "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert) 
   (if (string-equal (tempo-lookup-named 'gain) "") 
       (list 'l "GAIN=1e5")	;; default value 
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value  
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'p1) "") 
       (list 'l "P1=100")	;; default value
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rin) "") 
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   'n)   
 "opa1"
 "template for inserting an ELDO single output 1-pole linear opamp"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-linear-opa1d"
 '("Y"
   (p "[Instance name]: ") " OPAMP1D "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert) 
   (if (string-equal (tempo-lookup-named 'gain) "") 
       (list 'l "GAIN=1e5")	;; default value 
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value  
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'p1) "") 
       (list 'l "P1=100")	;; default value
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rin) "") 
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   (p "<Common mode rejection ratio>: " cmrr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cmrr) "") 
       (list 'l "CMRR=0.0")	;; default value 
     (list 'l "CMRR=" '(s cmrr)))
   '(just-one-space)
   'n)   
 "opa1d"
 "template for inserting an ELDO differential output 1-pole linear opamp"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-linear-opa2"
 '("Y"
   (p "[Instance name]: ") " OPAMP2 "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert) 
   (if (string-equal (tempo-lookup-named 'gain) "") 
       (list 'l "GAIN=1e5")	;; default value 
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value  
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'p1) "") 
       (list 'l "P1=100")	;; default value
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<Non-dominant pole>: " p2 'noinsert) 
   (if (string-equal (tempo-lookup-named 'p2) "") 
       (list 'l "P2=1e6")	;; default value
     (list 'l "P2=" '(s p2)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rin) "") 
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   'n)   
 "opa2"
 "template for inserting an ELDO single output 2-pole linear opamp"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-linear-opa2d"
 '("Y"
   (p "[Instance name]: ") " OPAMP2D "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert) 
   (if (string-equal (tempo-lookup-named 'gain) "") 
       (list 'l "GAIN=1e5")	;; default value 
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value  
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'p1) "") 
       (list 'l "P1=100")	;; default value
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<Non-dominant pole>: " p2 'noinsert) 
   (if (string-equal (tempo-lookup-named 'p2) "") 
       (list 'l "P2=1e6")	;; default value
     (list 'l "P2=" '(s p2)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rin) "") 
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   (p "<Common mode rejection ratio>: " cmrr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cmrr) "") 
       (list 'l "CMRR=0.0")	;; default value 
     (list 'l "CMRR=" '(s cmrr)))
   '(just-one-space)
   'n)   
 "opa2d"
 "template for inserting an ELDO differential output 2-pole linear opamp"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-delay"
 '("DEL"
   (p "[Instance name]: ") " "    
   (p "[Input]: ") " "
   (p "[Output]: ") " "
   (p "[Delay value]: ") " "
   'n)   
 "del"
 "template for inserting an ELDO delay"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-satr"
 '("Y"
   (p "[Instance name]: ") " SATR "    
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Value of resistance>: " r 'noinsert) 
   (if (string-equal (tempo-lookup-named 'r) "") 
       (list 'l "R=1")	;; default value 
     (list 'l "R=" '(s r)))
   '(just-one-space)
   (p "<Max current>: " imax 'noinsert) 
   (if (string-equal (tempo-lookup-named 'imax) "") 
       (list 'l "IMAX=1")	;; default value 
     (list 'l "IMAX=" '(s imax)))
   '(just-one-space)
   (p "<Slew rate(V/µs)>: " sr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'sr) "") 
       (list 'l "SR=0")	;; default value 
     (list 'l "SR=" '(s sr)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'p1) "") 
       (list 'l "P1=1e6")	;; default value 
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<resistance of low-pass filter>: " r1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'r1) "") 
       (list 'l "R1=30")	;; default value 
     (list 'l "R1=" '(s r1)))
   'n)   
 "satr"
 "template for inserting an ELDO saturating resistor"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-satv"
 '("Y"
   (p "[Instance name]: ") " SATV "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<VMax>: " vmax 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vmax) "") 
       (list 'l "VMAX=5.0")	;; default value 
     (list 'l "VMAX=" '(s vmax)))
   '(just-one-space)
   (p "<VMin>: " vmin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vmin) "") 
       (list 'l "VMIN=-5.0")	;; default value 
     (list 'l "VMIN=" '(s vmin)))
   '(just-one-space)
   (p "<Positive saturation voltage>: " vsatp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsatp) "") 
       (list 'l "VSATP=4.75")	;; default value 
     (list 'l "VSATP=" '(s vsatp)))
   '(just-one-space)
   (p "<Negative saturation voltage>: " vsatn 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsatn) "") 
       (list 'l "VSATN=-4.75")	;; default value 
     (list 'l "VSATN=" '(s vsatn)))
   '(just-one-space)
   (p "<Slope at VSATP>: " pslope 'noinsert) 
   (if (string-equal (tempo-lookup-named 'pslope) "") 
       (list 'l "PSLOPE=0.25")	;; default value 
     (list 'l "PSLOPE=" '(s pslope)))
   '(just-one-space)
   (p "<Slope at VSATN>: " nslope 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nslope) "") 
       (list 'l "NSLOPE=0.25")	;; default value 
     (list 'l "NSLOPE=" '(s nslope)))
   'n)   
 "satv"
 "template for inserting an ELDO voltage limiter"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-vswitch"
 '("Y"
   (p "[Instance name]: ") " VSWITCH "    
   (p "[Input]: ") " "
   (p "[Output]: ") " "
   (p "[Positive controlling node]: ") " "
   (p "[Negative controlling node]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert) 
   (if (string-equal (tempo-lookup-named 'level) "") 
       (list 'l "LEVEL=1")	;; default value 
     (list 'l "LEVEL=2" ))
   '(just-one-space)
   (p "<Voltage for 'ON' state>: " von 'noinsert) 
   (if (string-equal (tempo-lookup-named 'von) "") 
       (list 'l "VON=0.95")	;; default value 
     (list 'l "VON=" '(s von)))
   '(just-one-space)
   (p "<Voltage for 'OFF' state>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.05")	;; default value 
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<RON resistance>: " ron 'noinsert) 
   (if (string-equal (tempo-lookup-named 'ron) "") 
       (list 'l "RON=1e-2")	;; default value 
     (list 'l "RON=" '(s ron)))
   '(just-one-space)
   (p "<ROFF resistance>: " roff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'roff) "") 
       (list 'l "ROFF=1e10")	;; default value 
     (list 'l "ROFF=" '(s roff)))
   'n)   
 "vswitch"
 "template for inserting an ELDO voltage controlled switch"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-cswitch"
 '("Y"
   (p "[Instance name]: ") " CSWITCH "    
   (p "[Input]: ") " "
   (p "[Output]: ") " IC: "
   (p "[Controlling current]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert) 
   (if (string-equal (tempo-lookup-named 'level) "") 
       (list 'l "LEVEL=1")	;; default value 
     (list 'l "LEVEL=2" ))
   '(just-one-space)
   (p "<Current for 'ON' state>: " ion 'noinsert) 
   (if (string-equal (tempo-lookup-named 'ion) "") 
       (list 'l "ION=0.95")	;; default value 
     (list 'l "ION=" '(s ion)))
   '(just-one-space)
   (p "<Current for 'OFF' state>: " ioff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'ioff) "") 
       (list 'l "IOFF=0.05")	;; default value 
     (list 'l "IOFF=" '(s ioff)))
   '(just-one-space)
   (p "<RON resistance>: " ron 'noinsert) 
   (if (string-equal (tempo-lookup-named 'ron) "") 
       (list 'l "RON=1e-2")	;; default value 
     (list 'l "RON=" '(s ron)))
   '(just-one-space)
   (p "<ROFF resistance>: " roff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'roff) "") 
       (list 'l "ROFF=1e10")	;; default value 
     (list 'l "ROFF=" '(s roff)))
   'n)   
 "cswitch"
 "template for inserting an ELDO current controlled switch"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-tri2sin"
 '("Y"
   (p "[Instance name]: ") " TRI2SIN "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive output]: ") " "
   (p "[Negative output]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert) 
   (if (string-equal (tempo-lookup-named 'level) "") 
       (list 'l "LEVEL=1")	;; default value 
     (list 'l "LEVEL=" '(s level)))
   '(just-one-space)   
   (p "<Gain>: " gain 'noinsert) 
   (if (string-equal (tempo-lookup-named 'gain) "") 
       (list 'l "GAIN=1e5")	;; default value 
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value  
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Upper input voltage limit >: " vu 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vu) "") 
       (list 'l "VU=1")		;; default value
     (list 'l "VU=" '(s vu)))
   '(just-one-space)
   (p "<Lower input voltage limit >: " vl 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vl) "") 
       (list 'l "VL=1")		;; default value
     (list 'l "VL=" '(s vl)))
   'n)   
 "tri2sin"
 "template for inserting an ELDO triangular to sine wave converter"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-stairgen"
 '("Y"
   (p "[Instance name]: ") " STAIRGEN "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " param: "
   (p "<Start voltage>: " vstart 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vstart) "") 
       (list 'l "VSTART=0.0")	;; default value 
     (list 'l "VSTART=" '(s vstart)))
   '(just-one-space)
   (p "<Step voltage>: " vdelta 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vdelta) "") 
       (list 'l "VDELTA=0.1")	;; default value 
     (list 'l "VDELTA=" '(s vdelta)))
   '(just-one-space)
   (p "<Number of steps>: " nstep 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nstep) "") 
       (list 'l "NSTEP=10")	;; default value 
     (list 'l "NSTEP=" '(s nstep)))
   '(just-one-space)
   (p "<Period>: " tdu 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tdu) "") 
       (list 'l "TDU=1e-4")	;; default value 
     (list 'l "TDU=" '(s tdu)))
   '(just-one-space)
   (p "<Slew rate (V/µs)>: " slr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'slr) "") 
       (list 'l "SLR=1")	;; default value 
     (list 'l "SLR=" '(s slr)))
   'n)   
 "stairgen"
 "template for inserting an ELDO staircase waveform generator"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-sawgen"
 '("Y"
   (p "[Instance name]: ") " SAWGEN "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " param: "
   (p "<Start voltage>: " v0 'noinsert) 
   (if (string-equal (tempo-lookup-named 'v0) "") 
       (list 'l "V0=0.0")	;; default value 
     (list 'l "V0=" '(s v0)))
   '(just-one-space)
   (p "<Voltage magnitude>: " v1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'v1) "") 
       (list 'l "V1=5.0")	;; default value 
     (list 'l "V1=" '(s v1)))
   '(just-one-space)
   (p "<Period>: " tdu 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tdu) "") 
       (list 'l "TDU=1e-4")	;; default value 
     (list 'l "TDU=" '(s tdu)))
   '(just-one-space)
   (p "<Delay>: " tdel 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tdel) "") 
       (list 'l "TDEL=0.0")	;; default value 
     (list 'l "TDEL=" '(s tdel)))
   'n)   
 "sawgen"
 "template for inserting an ELDO sawtooth waveform generator"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-trigen"
 '("Y"
   (p "[Instance name]: ") " TRIGEN "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " param: "
   (p "<Start voltage>: " v0 'noinsert) 
   (if (string-equal (tempo-lookup-named 'v0) "") 
       (list 'l "V0=0.0")	;; default value 
     (list 'l "V0=" '(s v0)))
   '(just-one-space)
   (p "<Voltage magnitude>: " v1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'v1) "") 
       (list 'l "V1=5.0")	;; default value 
     (list 'l "V1=" '(s v1)))
   '(just-one-space)
   (p "<First edge duration>: " rdu 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rdu) "") 
       (list 'l "RDU=1e-4")	;; default value 
     (list 'l "RDU=" '(s rdu)))
   '(just-one-space)
   (p "<Second edge duration>: " fdu 'noinsert) 
   (if (string-equal (tempo-lookup-named 'fdu) "") 
       (list 'l "FDU=1e-4")	;; default value 
     (list 'l "FDU=" '(s fdu)))
   '(just-one-space)
   (p "<Delay>: " tdel 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tdel) "") 
       (list 'l "TDEL=0.0")	;; default value 
     (list 'l "TDEL=" '(s tdel)))
   'n)   
 "trigen"
 "template for inserting an ELDO triangular waveform generator"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-amm"
 '("Y"
   (p "[Instance name]: ") " AMM "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert) 
   (if (string-equal (tempo-lookup-named 'level) "") 
       (list 'l "LEVEL=1")	;; default value 
     (list 'l "LEVEL=" '(s level)))
   '(just-one-space)
   (p "<Slewrate (V/µs)>: " slr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'slr) "") 
       (list 'l "SLR=10")	;; default value 
     (list 'l "SLR=" '(s slr)))
   '(just-one-space)
   (p "<Offset voltage>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value 
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Carrier frequency>: " fc 'noinsert) 
   (if (string-equal (tempo-lookup-named 'fc) "") 
       (list 'l "FC=1e6")	;; default value 
     (list 'l "FC=" '(s fc)))
   '(just-one-space)
   (p "<Minimal number of sampling points per period>: " nsam 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nsam) "") 
       (list 'l "NSAM=10")	;; default value 
     (list 'l "NSAM=" '(s nsam)))
   'n)   
 "amm"
 "template for inserting an ELDO amplitude modulator"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-pam"
 '("Y"
   (p "[Instance name]: ") " PAM "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert) 
   (if (string-equal (tempo-lookup-named 'level) "") 
       (list 'l "LEVEL=1")	;; default value 
     (list 'l "LEVEL=" '(s level)))
   '(just-one-space)
   (p "<Slewrate (V/µs)>: " slr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'slr) "") 
       (list 'l "SLR=10")	;; default value 
     (list 'l "SLR=" '(s slr)))
   '(just-one-space)
   (p "<Offset voltage>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value 
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Carrier frequency>: " fc 'noinsert) 
   (if (string-equal (tempo-lookup-named 'fc) "") 
       (list 'l "FC=1e6")	;; default value 
     (list 'l "FC=" '(s fc)))
   '(just-one-space)
   (p "<Minimal number of sampling points per period>: " nsam 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nsam) "") 
       (list 'l "NSAM=10")	;; default value 
     (list 'l "NSAM=" '(s nsam)))
   'n)   
 "pam"
 "template for inserting an ELDO pulse amplitude modulator"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-saho"
 '("Y"
   (p "[Instance name]: ") " SA_HO "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<Sampling frequency>: " fs 'noinsert) 
   (if (string-equal (tempo-lookup-named 'fs) "") 
       (list 'l "FS=1e6")	;; default value 
     (list 'l "FS=" '(s fs)))
   '(just-one-space)
   (p "<Acquisition time>: " tacq 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tacq) "") 
       (list 'l "TACQ=1e-9")	;; default value 
     (list 'l "TACQ=" '(s tacq)))
   '(just-one-space)
   (p "<Droop voltage>: " dv 'noinsert) 
   (if (string-equal (tempo-lookup-named 'dv) "") 
       (list 'l "DV=20mv")	;; default value 
     (list 'l "DV=" '(s dv)))
   '(just-one-space)
   'n)   
 "saho"
 "template for inserting an ELDO sample&hold"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-trho"
 '("Y"
   (p "[Instance name]: ") " TR_HO "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Controlling node]: ") " param: "
   (p "<Threshold voltage for CRT>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=0.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Acquisition time>: " tacq 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tacq) "") 
       (list 'l "TACQ=1e-9")	;; default value 
     (list 'l "TACQ=" '(s tacq)))
   'n)   
 "trho"
 "template for inserting an ELDO track&hold"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-peakd"
 '("Y"
   (p "[Instance name]: ") " PEAK_D "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Controlling node]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert) 
   (if (string-equal (tempo-lookup-named 'level) "") 
       (list 'l "LEVEL=1")	;; default value 
     (list 'l "LEVEL=2" ))
   '(just-one-space)
   (p "<Threshold voltage for CRT>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=0.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold voltage for reset on output>: " res 'noinsert) 
   (if (string-equal (tempo-lookup-named 'res) "") 
       (list 'l "RES=0.5")	;; default value 
     (list 'l "RES=" '(s res)))
   '(just-one-space)
   (p "<Output slewrate (V/µs)>: " slr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'slr) "") 
       (list 'l "SLR=1.0")	;; default value 
     (list 'l "SLR=" '(s slr)))
   '(just-one-space)
   (p "<Output slewrate on reset>: " rslr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rslr) "") 
       (list 'l "RSLR=1.0")	;; default value 
     (list 'l "RSLR=" '(s rslr)))
   'n)   
 "peakd"
 "template for inserting an ELDO peak detector"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-levdso"
 '("Y"
   (p "[Instance name]: ") " LEV_D "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<Rise time (µs)>: " tr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tr) "") 
       (list 'l "TR=1.0")	;; default value 
     (list 'l "TR=" '(s tr)))
   '(just-one-space)
   (p "<Fall time (µs)>: " tf 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tf) "") 
       (list 'l "TF=1.0")	;; default value 
     (list 'l "TF=" '(s tf)))
   '(just-one-space)
   (p "<Transit time (s)>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=0.0")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Lower voltage level>: " v0 'noinsert) 
   (if (string-equal (tempo-lookup-named 'v0) "") 
       (list 'l "V0=0.0")	;; default value 
     (list 'l "V0=" '(s v0)))
   '(just-one-space)
   (p "<Higher voltage level>: " v1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'v1) "") 
       (list 'l "V1=1.0")	;; default value 
     (list 'l "V1=" '(s v1)))
   '(just-one-space)
   (p "<Input offset voltage>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value 
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Lower reference voltage>: " vrl 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vrl) "") 
       (list 'l "VRL=-0.1")	;; default value 
     (list 'l "VRL=" '(s vrl)))
   '(just-one-space)
   (p "<Higher reference voltage>: " vru 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vru) "") 
       (list 'l "VRU=0.1")	;; default value 
     (list 'l "VRU=" '(s vru)))
   'n)   
 "levdso"
 "template for inserting an ELDO single-output level detector"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-levddo"
 '("Y"
   (p "[Instance name]: ") " LEV_D "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Reference node]: ") " param: "
   (p "<Rise time (µs)>: " tr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tr) "") 
       (list 'l "TR=1.0")	;; default value 
     (list 'l "TR=" '(s tr)))
   '(just-one-space)
   (p "<Fall time (µs)>: " tf 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tf) "") 
       (list 'l "TF=1.0")	;; default value 
     (list 'l "TF=" '(s tf)))
   '(just-one-space)
   (p "<Transit time (s)>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=0.0")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Lower voltage level>: " v0 'noinsert) 
   (if (string-equal (tempo-lookup-named 'v0) "") 
       (list 'l "V0=0.0")	;; default value 
     (list 'l "V0=" '(s v0)))
   '(just-one-space)
   (p "<Higher voltage level>: " v1 'noinsert) 
   (if (string-equal (tempo-lookup-named 'v1) "") 
       (list 'l "V1=1.0")	;; default value 
     (list 'l "V1=" '(s v1)))
   '(just-one-space)
   (p "<Input offset voltage>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value 
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Lower reference voltage>: " vrl 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vrl) "") 
       (list 'l "VRL=-0.1")	;; default value 
     (list 'l "VRL=" '(s vrl)))
   '(just-one-space)
   (p "<Higher reference voltage>: " vru 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vru) "") 
       (list 'l "VRU=0.1")	;; default value 
     (list 'l "VRU=" '(s vru)))
   'n)   
 "levddo"
 "template for inserting an ELDO differential-output level detector"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-logamp"
 '("Y"
   (p "[Instance name]: ") " LOGAMP "    
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Gain>: " gain 'noinsert) 
   (if (string-equal (tempo-lookup-named 'gain) "") 
       (list 'l "K=1.0")	;; default value 
     (list 'l "K=" '(s gain)))
   '(just-one-space)
   (p "<Log function argument>: " e 'noinsert) 
   (if (string-equal (tempo-lookup-named 'e) "") 
       (list 'l "E=1")	;; default value 
     (list 'l "E=" '(s vmin)))
   '(just-one-space)
   (p "<Vmax>: " vmax 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vmax) "") 
       (list 'l "VMAX=5.0")	;; default value 
     (list 'l "VMAX=" '(s vmax)))
   '(just-one-space)
   (p "<Vmin>: " vmin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vmin) "") 
       (list 'l "VMIN=-5.0")	;; default value 
     (list 'l "VMIN=" '(s vmin)))
   '(just-one-space)
   (p "<Positive saturation voltage>: " vsatp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsatp) "") 
       (list 'l "VSATP=4.75")	;; default value 
     (list 'l "VSATP=" '(s vsatp)))
   '(just-one-space)
   (p "<Negative saturation voltage>: " vsatn 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsatn) "") 
       (list 'l "VSATN=-4.75")	;; default value 
     (list 'l "VSATN=" '(s vsatn)))
   '(just-one-space)
   (p "<Slope at VSATP>: " pslope 'noinsert) 
   (if (string-equal (tempo-lookup-named 'pslope) "") 
       (list 'l "PSLOPE=0.25")	;; default value 
     (list 'l "PSLOPE=" '(s pslope)))
   '(just-one-space)
   (p "<Slope at VSATN>: " nslope 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nslope) "") 
       (list 'l "NSLOPE=0.25")	;; default value 
     (list 'l "NSLOPE=" '(s nslope)))
   'n)   
 "logamp"
 "template for inserting an ELDO logarithmic amplifier"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-antilog"
 '("Y"
   (p "[Instance name]: ") " EXPAMP "    
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Gain>: " k 'noinsert) 
   (if (string-equal (tempo-lookup-named 'k) "") 
       (list 'l "K=1.0")	;; default value 
     (list 'l "K=" '(s k)))
   '(just-one-space)
   (p "<Exp function argument>: " e 'noinsert) 
   (if (string-equal (tempo-lookup-named 'e) "") 
       (list 'l "E=1")		;; default value 
     (list 'l "E=" '(s vmin)))
   '(just-one-space)
   (p "<Base of power function>: " base 'noinsert) 
   (if (string-equal (tempo-lookup-named 'base) "") 
       (list 'l "BASE={EXP(1)}")	;; default value 
     (list 'l "BASE=" '(s base)))
   '(just-one-space)
   (p "<Vmax>: " vmax 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vmax) "") 
       (list 'l "VMAX=5.0")	;; default value 
     (list 'l "VMAX=" '(s vmax)))
   '(just-one-space)
   (p "<Vmin>: " vmin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vmin) "") 
       (list 'l "VMIN=-5.0")	;; default value 
     (list 'l "VMIN=" '(s vmin)))
   '(just-one-space)
   (p "<Positive saturation voltage>: " vsatp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsatp) "") 
       (list 'l "VSATP=4.75")	;; default value 
     (list 'l "VSATP=" '(s vsatp)))
   '(just-one-space)
   (p "<Negative saturation voltage>: " vsatn 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsatn) "") 
       (list 'l "VSATN=-4.75")	;; default value 
     (list 'l "VSATN=" '(s vsatn)))
   '(just-one-space)
   (p "<Slope at VSATP>: " pslope 'noinsert) 
   (if (string-equal (tempo-lookup-named 'pslope) "") 
       (list 'l "PSLOPE=0.25")	;; default value 
     (list 'l "PSLOPE=" '(s pslope)))
   '(just-one-space)
   (p "<Slope at VSATN>: " nslope 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nslope) "") 
       (list 'l "NSLOPE=0.25")	;; default value 
     (list 'l "NSLOPE=" '(s nslope)))
   'n)   
 "expamp"
 "template for inserting an ELDO anti-logarithmic amplifier"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-diff"
 '("Y"
   (p "[Instance name]: ") " DIFF "    
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Time constant>: " k 'noinsert) 
   (if (string-equal (tempo-lookup-named 'k) "") 
       (list 'l "K=1")		;; default value 
     (list 'l "K=" '(s k)))
   '(just-one-space)
   (p "<DC value>: " c0 'noinsert) 
   (if (string-equal (tempo-lookup-named 'c0) "") 
       (list 'l "C0=1")		;; default value 
     (list 'l "C0=" '(s c0)))
   '(just-one-space)
   (p "<Slewrate (V/s)>: " slr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'slr) "") 
       (list 'l "SLR=1e9")	;; default value 
     (list 'l "SLR=" '(s slr)))
   'n)   
 "diff"
 "template for inserting an ELDO differentiator"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-integ"
 '("Y"
   (p "[Instance name]: ") " INTEG "    
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Time constant>: " k 'noinsert) 
   (if (string-equal (tempo-lookup-named 'k) "") 
       (list 'l "K=1")		;; default value 
     (list 'l "K=" '(s k)))
   '(just-one-space)
   (p "<DC value>: " c0 'noinsert) 
   (if (string-equal (tempo-lookup-named 'c0) "") 
       (list 'l "C0=1")		;; default value 
     (list 'l "C0=" '(s c0)))
   'n)   
 "integ"
 "template for inserting an ELDO integrator"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-adder"
 '("Y"
   (p "[Instance name]: ")  " "   
   (p "<ADD/SUB/MULT/DIV>: ") " "
   (p "[Input 2]: ") " "
   (p "[Output]: ") " param: "
   (p "<Vmax>: " vmax 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vmax) "") 
       (list 'l "VMAX=5.0")	;; default value 
     (list 'l "VMAX=" '(s vmax)))
   '(just-one-space)
   (p "<Vmin>: " vmin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vmin) "") 
       (list 'l "VMIN=-5.0")	;; default value 
     (list 'l "VMIN=" '(s vmin)))
   '(just-one-space)
   (p "<Positive saturation voltage>: " vsatp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsatp) "") 
       (list 'l "VSATP=4.75")	;; default value 
     (list 'l "VSATP=" '(s vsatp)))
   '(just-one-space)
   (p "<Negative saturation voltage>: " vsatn 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsatn) "") 
       (list 'l "VSATN=-4.75")	;; default value 
     (list 'l "VSATN=" '(s vsatn)))
   '(just-one-space)
   (p "<Slope at VSATP>: " pslope 'noinsert) 
   (if (string-equal (tempo-lookup-named 'pslope) "") 
       (list 'l "PSLOPE=0.25")	;; default value 
     (list 'l "PSLOPE=" '(s pslope)))
   '(just-one-space)
   (p "<Slope at VSATN>: " nslope 'noinsert) 
   (if (string-equal (tempo-lookup-named 'nslope) "") 
       (list 'l "NSLOPE=0.25")	;; default value 
     (list 'l "NSLOPE=" '(s nslope)))
   'n)   
 "add"
 "template for inserting an ELDO adder/subtrator/multiplier/divider"
 'eldo-tempo-tags
)

;; -------------------
;; Digital Macromodels
;; -------------------

(tempo-define-template
 "eldo-inv"
 '("INV"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "inv"
 "template for inserting an ELDO INVERTER gate macromodel"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-xor"
 '("XOR"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "xor"
 "template for inserting an ELDO Exclusive-OR gate macromodel"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-and2"
 '("AND"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "and"
 "template for inserting an ELDO 2 input AND gate macromodel"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-nand2"
 '("NAND"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "nand"
 "template for inserting an ELDO 2 input NAND gate macromodel"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-or2"
 '("OR"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "or"
 "template for inserting an ELDO 2 input OR gate macromodel"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-nor2"
 '("NOR"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "nor"
 "template for inserting an ELDO 2 input NOR gate macromodel"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-and3"
 '("AND3"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "and3"
 "template for inserting an ELDO 3 input AND gate macromodel"
 'eldo-tempo-tags
)


(tempo-define-template
 "eldo-nand3"
 '("NAND3"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "nand3"
 "template for inserting an ELDO 3 input NAND gate macromodel"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-or3"
 '("OR3"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "or3"
 "template for inserting an ELDO 3 input OR gate macromodel"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-nor3"
 '("NOR3"
   (p "[Instance name]: ") " "    
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value 
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vthi) "") 
       (list 'l "")	;; default value 
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtlo) "") 
       (list 'l "")	;; default value 
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=1.0ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdup) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpdown) "") 
       (list 'l "")	;; default value 
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)   
 "nor3"
 "template for inserting an ELDO 3 input NOR gate macromodel"
 'eldo-tempo-tags
)

;; -------------------------
;; Mixed signal Macromodels
;; -------------------------

(tempo-define-template
 "eldo-adc"
 '("ADC"
   (p "[Instance name]: ") " "    
   (p "[Clock]: ") " "
   (p "[Analog input]: ") " "
   (p "[Digital Outputs from MSB to LSB]: ") " "
   (p "<Edge (1/-1)>: " edge 'noinsert) 
   (if (string-equal (tempo-lookup-named 'edge) "") 
       (list 'l "EDGE=1")	;; default value 
     (list 'l "EDGE=-1"))
   '(just-one-space)
   (p "<Threshold clock voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)  
   (p "<Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space) 
   (p "<Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Analog input lower voltage>: " vinf 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vinf) "") 
       (list 'l "VINF=0.0")	;; default value 
     (list 'l "VTHI=" '(s vinf)))
   '(just-one-space)
   (p "<Analog input higher voltage>: " vsup 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsup) "") 
       (list 'l "VSUP=5.0")	;; default value 
     (list 'l "VSUP=" '(s vsup)))
   '(just-one-space)
   (p "<Output bits commutation time>: " tcom 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tcom) "") 
       (list 'l "TCOM=1.0ns")	;; default value 
     (list 'l "TCOM=" '(s tcom)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=10ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   'n)   
 "adc"
 "template for inserting an ELDO Analog to Digital Converter  macromodel"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-dac"
 '("DAC"
   (p "[Instance name]: ") " "    
   (p "[Clock]: ") " "
   (p "[Digital inputs from MSB to LSB]: ") " "
   (p "[Analog output]: ") " "
   (p "<Edge (1/-1)>: " edge 'noinsert) 
   (if (string-equal (tempo-lookup-named 'edge) "") 
       (list 'l "EDGE=1")	;; default value 
     (list 'l "EDGE=-1"))
   '(just-one-space)
   (p "<Threshold clock voltage>: " vth 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vth) "") 
       (list 'l "VTH=2.5")	;; default value 
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vtin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vtin) "") 
       (list 'l "VTIN=2.5")	;; default value 
     (list 'l "VTIN=" '(s vtin)))
   '(just-one-space)  
   (p "<Analog Output Vhigh>: " vhi 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vhi) "") 
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space) 
   (p "<Analog Output Vlow>: " vlo 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vlo) "") 
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert) 
   (if (string-equal (tempo-lookup-named 'tpd) "") 
       (list 'l "TPD=10ns")	;; default value 
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Output slope (V/s)>: " sl 'noinsert) 
   (if (string-equal (tempo-lookup-named 'sl) "") 
       (list 'l "SL=10e8")	;; default value 
     (list 'l "SL=" '(s tcom)))
   '(just-one-space)
   'n)   
 "DAC"
 "template for inserting an ELDO Digital to Analog Converter macromodel"
 'eldo-tempo-tags
)

;; -------------------------
;; Switched cap Macromodels
;; -------------------------

(tempo-define-template
 "eldo-switchcap-opa"
 '("OPA"
   (p "[Instance name]: ") " "    
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Input offset>: " voff 'noinsert) 
   (if (string-equal (tempo-lookup-named 'voff) "") 
       (list 'l "VOFF=0.0")	;; default value  
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Slew rate (V/s)>: " sl 'noinsert) 
   (if (string-equal (tempo-lookup-named 'sl) "") 
       (list 'l "SL=1e6")	;; default value  
     (list 'l "SL=" '(s voff)))
   '(just-one-space)
   (p "<Gain>: " gain 'noinsert) 
   (if (string-equal (tempo-lookup-named 'gain) "") 
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cin) "") 
       (list 'l "CIN=0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   (p "<Output resistance>: " rs 'noinsert) 
   (if (string-equal (tempo-lookup-named 'rs) "") 
       (list 'l "RS=10e6")	;; default value
     (list 'l "RS=" '(s rs)))
   '(just-one-space)
   (p "<Symmetrical saturation voltage>: " vsat 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsat) "") 
       (list 'l "VSAT=5.0")	;; default value 
     (list 'l "VSAT=" '(s vsat)))
   '(just-one-space)
   (p "<Asymmetrical saturation voltage>: " vsatm 'noinsert) 
   (if (string-equal (tempo-lookup-named 'vsatm) "") 
       (list 'l "VSATM=5.0")	;; default value 
     (list 'l "VSATM=" '(s vsatm)))
   '(just-one-space)
   (p "<Cutoff frequency (double stage only)>: " fc 'noinsert) 
   (if (string-equal (tempo-lookup-named 'fc) "") 
       (list 'l "FC=1k")	;; default value 
     (list 'l "FC=" '(s fc)))
   '(just-one-space)
   (p "<Non-dominant pole (single stage only)>: " fndp 'noinsert) 
   (if (string-equal (tempo-lookup-named 'fndp) "") 
       (list 'l "FNDP=1k")	;; default value 
     (list 'l "FNDP=" '(s fndp)))
   '(just-one-space)
   (p "<Max current>: " imax 'noinsert) 
   (if (string-equal (tempo-lookup-named 'imax) "") 
       (list 'l "IMAX=100mA")	;; default value 
     (list 'l "IMAX=" '(s imax)))
   '(just-one-space)
   (p "<Common mode rejection ratio>: " cmrr 'noinsert) 
   (if (string-equal (tempo-lookup-named 'cmrr) "") 
       (list 'l "CMRR=0.0")	;; default value 
     (list 'l "CMRR=" '(s cmrr)))
   'n)   
 "opa"
 "template for inserting an ELDO differential single or double stage opamp"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-switch"
 '("S"
   (p "[Instance name]: ") " "    
   (p "[Controlling node]: ") " "
   (p "[Node 1]: ") " "
   (p "[Node 2]: ") " "
   (p "[Model name]: ") " "
   (p "<RON resistance>: " ron 'noinsert) 
   (if (string-equal (tempo-lookup-named 'ron) "") 
       (list 'l "RON=1k")	;; default value 
     (list 'l "RON=" '(s ron)))
   '(just-one-space)
   (p "<Overlap capacitance>: " crec 'noinsert) 
   (if (string-equal (tempo-lookup-named 'crec) "") 
       (list 'l "CREC=0")	;; default value 
     (list 'l "CREC=" '(s crec)))
   'n)   
 "switch"
 "template for inserting an ELDO switch macromodel"
 'eldo-tempo-tags
)


;; -------------------
;; Extracts   
;; -------------------

(tempo-define-template
 "eldo-phmag"
 '(".EXTRACT AC label=\"Phase margin\" xycond(vp("
   (p "[Node]: " lname)    
   "),vdb(" (s lname) ")<0.0)+180 "   
   'n)
 "phmag"
 "template for extracting the phase margin"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-pmmin"
 '(".EXTRACT AC label=\"PM min\" min(vp("
   (p "[Node]: " lname)    
   "),0,xdown(vdb(" (s lname) "),0))+180 "   
   'n)
 "pmmin"
 "template for extracting the minimal phase before unity gain frequency"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-gmag"
 '(".EXTRACT AC label=\"Gain margin\" -xycond(vdb("
   (p "[Node]: " lname)    
   "),vp(" (s lname) ")<-180) "   
   'n)
 "gmag"
 "template for extracting the gain margin"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-fc"
 '(".EXTRACT AC label=\"Cut freq\" xdown(vdb("
   (p "[Node]: " lname)    
   "),yval(vdb(" (s lname) "),1)-3) "   
   'n)
 "fc"
 "template for extracting the cut frequency"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-ugfc"
 '(".EXTRACT AC label=\"Unity gain freq\" xdown(vdb("
   (p "[Node]: " lname)    
   "),0) "   
   'n)
 "ugfc"
 "template for extracting the unity gain frequency"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-period"
 '(".EXTRACT TRAN xdown(v("
   (p "[Node]: " lname)    
   "),"
   (p "[threshold]: " vth)
   ","
   (p "[estimation time]: " t)
   ",end)"
   "-xdown(v(" (s lname) "),"(s vth) ","(s t) ",start) !period"   
   'n)
 "period"
 "template for extracting the period of a signal"
 'eldo-tempo-tags
)


;; Some macros (defmac)


(tempo-define-template
 "eldo-period-macro"
 '(".DEFMAC period(a,th,time)=xdown(a,th,time,end)"
   "-xdown(a,th,time,start)"   
   'n)
 "period"
 "macro for extracting the period of signal a"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-duty-macro"
 '(".DEFMAC duty_cycle(a,th,time)=(xdown(a,th,time,end)"
   "-xup(a,th,time,end))/(xdown(a,th,time,end)-xdown(a,th,time,start))*100"
   'n)
 "duty"
 "macro for extracting the duty cycle of signal a"
 'eldo-tempo-tags
)

(tempo-define-template
 "eldo-settling-macro"
 '(".DEFMAC settling(xaxis,a,ratio,Tstart,Tfinal)=xycond(xaxis,(a>(yval(a,Tfinal)*(1+ratio)))"
   " || (a<(yval(a,Tfinal)*(1-ratio))),Tfinal,Tstart) - Tstart"
   'n)
 "settling"
 "macro for extracting the settling cycle of signal A, within ±ratio of value of A at time Tfinal"
 'eldo-tempo-tags
)


;; SUBCKT and HEADER

(tempo-define-template
 "eldo-subckt"
 '(".SUBCKT "
   (p "[subckt name]: " lname) 'r 'n 'n
   ".ENDS " (s lname)  '>)
 "subckt"
 "template for inserting an Eldo subckt"
 'eldo-tempo-tags
)



(tempo-define-template
 "eldo-circuit-header"
 '('eldo-default-header
   'n)
 "header"
 "template for inserting a header for a circuit"
 'eldo-tempo-tags
)



;;------------------------------------------------------------
;; Changelog and sections support
;;------------------------------------------------------------

(defconst eldo-section-regexp-start "^[*]*!\\s-*"
  "Eldo mode section header start regexp.")


(defvar eldo-section-headings nil
  "List of eldo mode section headings.")


(setq eldo-section-headings (list "Changelog")) ; Changelog is special case
(let ((section-alist eldo-section-alist) heading)
  (while section-alist
    (setq heading (car (cdr (car section-alist))))
    (setq eldo-section-headings (append eldo-section-headings
					(list heading)))
    (setq section-alist (cdr section-alist))))


(defun eldo-add-changelog-entry (changelog-entry)
  "Find changelog section (create it if not found) and add an entry for today."
  (interactive "sChangelog entry: ")
  (goto-char (point-min))
  (if (not (re-search-forward "^\![\t ]*Changelog" nil t))
      (eldo-add-section "Changelog" (point-max) ))

  (eldo-goto-section "Changelog")
  ;(forward-line 2)
  (let ((string (concat "* " (substring (current-time-string) 0 11)
			(substring (current-time-string) -4) " "
			(user-full-name) " <" user-mail-address ">")))
    (if (not (search-forward string nil t))
	(insert "\n" string "\n\n")
      (forward-line 2))
    (insert "    - " changelog-entry "\n")))
      

(defun eldo-goto-section (section)
  "Move point to the beginning of the specified section;    
If the section is not found, leave point at previous location."
  (interactive "ssection: ")
  (let ((pos (point)))
    (goto-char (point-min))
    (if (not (re-search-forward   
	      (concat eldo-section-regexp-start section "\\b") nil t))
	(progn (message "Couldn't find section %s" section)
	       (goto-char pos)
	       )   
      (progn
	(forward-line 2)
	(recenter))))) ;; added recenter


(defun eldo-add-section (section &optional arg)
  "Add a section in buffer at (optionnal) point arg"
  (interactive "ssection:")
  (if arg   
      (goto-char arg))
  (eldo-comment-bar)
  (insert   
   (concat "!\t" section " \n"))
  (eldo-comment-bar))


(defun eldo-section-p (section)
  "checks if named section is in file, returns t if found, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward   
     (concat eldo-section-regexp-start section "\\b") nil t)
    ))


;;------------------------------------------------------------
;; File initialization support
;;------------------------------------------------------------
            
(defun eldo-file-initialize ()
  "Create a template for new file"
  (interactive)
  (if (file-exists-p "~/.eldo")
      (insert-file-contents "~/.eldo")
    
    (insert   
     (concat "# " (buffer-name)
	     "\n.notrc\n.nocom"   
	     "\n\n\n"   
	     eldo-default-header
	     "\n\n\n\n"))
    (eldo-add-section "LIBRARIES")
    (insert "\n\n\n\n")
    (eldo-add-section "SIMULATION OPTIONS")
    (insert   
     (concat "\n\n\n"
	     ".options STAT=1 SIMUDIV=10 !Status reports\n"   
	     ".options noascii nomod    \n"   
	     ".options eps=1e-7 itol=1e-6 gmin=1e-16 analog \n"
	     ".options nobound_phase"   
	     ".width out=80 \n"   
	     ".temp=27 \n"   
	     "\n\n\n\n"))
    (eldo-add-section "INPUT SIGNALS")
    (insert  "\n\n\n\n.END\n\n\n\n")
    (eldo-add-section "CHANGELOG")
    (insert "\n\n\n\n*** Local Variables:\n*** mode:eldo\n*** End:\n")
    )
  (eldo-add-changelog-entry "File created")
  )


;;------------------------------------------------------------
;; Support for automatic loading of library files on mouse click
;;------------------------------------------------------------

;;------------------------------------------------------------
;; Mouse bindings (only used by 'eldo-load-file-at-mouse')
;; I separate this from eldo-mode-map so that this particular
;; mouse binding doesn't interfere with other bindings


(if eldo-running-xemacs
    (require 'overlay)
  (require 'lucid)) ;; what else can we do ??

(defconst eldo-library-regexp-start
  ;;"^\\s-*\\.\\(include\\|lib\\|libfas\\)\\s-+\\(?:key=\\w+\\s-\\)*"
  "^\\s-*\\.\\(include\\|lib\\|libfas\\)\\s-+\\(?:key=\\w+\\s-\\)*'?"
  "Regexp that matches the beginning of library or include filename")

(defconst eldo-library-regexp-end
  "\\([^ \t\n']*\\)"
  ;;"\\s-*\\<\\(\\w+\\)\\>" 
  "Regexp that matches the end of library or include filename")

(defvar eldo-mode-mouse-map nil
  "Map containing mouse bindings for eldo-mode.")

(if eldo-mode-mouse-map   
    ()
  (setq eldo-mode-mouse-map (make-sparse-keymap))
  (set-keymap-parent eldo-mode-mouse-map eldo-mode-map)
  ; mouse button bindings
  (define-key eldo-mode-mouse-map "\r" 'ffap)
  (if (string-match "XEmacs" emacs-version)
      (define-key eldo-mode-mouse-map 'button2 'eldo-load-file-at-mouse)
    (define-key eldo-mode-mouse-map [mouse-2] 'eldo-load-file-at-mouse))
  (if (string-match "XEmacs" emacs-version)
      (define-key eldo-mode-mouse-map 'Sh-button2 'mouse-yank)
    (define-key eldo-mode-mouse-map [S-mouse-2] 'mouse-yank-at-click))
  )

;; create set-extent-keymap procedure when it does not exist
(eval-and-compile
  (unless (fboundp 'set-extent-keymap)
    (defun set-extent-keymap (extent keymap)
      (set-extent-property extent 'local-map keymap)
      )
    )
  )


(defun eldo-colorize-libraries (beg end old-len)
  "This function colorises libraries and included files when the mouse
passes over them. Clicking on the middle-mouse button loads them in a buffer.
BEWARE, this feature was hard to implement, and contains (non-fatal) bugs
primarily because emacs 20 does not have the same support for this as xemacs
does."
  (save-excursion
    (let (end-point)
      (goto-char end)
      (end-of-line)
      (setq end-point (point))
      (goto-char beg)
      (beginning-of-line)  ; scan entire line !
      ; delete overlays existing on this line   
      (let ((overlays (overlays-in (point) end-point)))
	(while overlays
	  (if (overlay-get (car overlays) 'detachable)
	      (delete-overlay (car overlays))
	    )
	  (setq overlays (cdr overlays))
	  )
	)
      ; make new ones, could reuse deleted one ?
      (while (search-forward-regexp eldo-library-regexp-start end-point t)
	( let (start-lib extent)
	  (setq start-lib (point))
	  (search-forward-regexp eldo-library-regexp-end end-point)
					; (let ((end-lib (point)))
	  (or (extent-at (point) (buffer-name) 'mouse-face) ;; not yet extended
	      (progn
		(setq extent (make-extent start-lib (point)))
		(set-extent-property extent 'start-closed t)
		(set-extent-property extent 'end-closed t)
		(set-extent-property extent 'detachable t)
		(set-extent-property extent 'mouse-face 'highlight)
		(set-extent-keymap extent eldo-mode-mouse-map)))))))
  )

(defun eldo-colorize-libraries-buffer ()
  (interactive)
  ; delete overlays
  (let ((overlays (overlays-in (point-min) (point-max))))
    (while overlays
      (if (overlay-get (car overlays) 'detachable)
	  (delete-overlay (car overlays))
	)
      (setq overlays (cdr overlays))
      )
    )
  ; remake overlays
  (eldo-colorize-libraries  (point-min) (point-max) nil)
  )

;; ffap-at-mouse isn't available in xemacs < 21
;; so define this function to do more or less the same....
(defun eldo-load-file-at-mouse (event)
  "loads file under button 2 click. Checks if file is readable."
    (interactive "@e")
    (if (fboundp 'ffap-at-mouse)
	(ffap-at-mouse event)  ;; use ffap-at-mouse if available
      (save-excursion
	(mouse-set-point event)
	(beginning-of-line)
	(if (looking-at (concat eldo-library-regexp-start   
				eldo-library-regexp-end))
	    (progn
	      (setq filename (expand-file-name
			      (substitute-in-file-name (match-string 2))))
	      ;;(message "filename is %s" filename)
	      (if (file-readable-p filename)
		  (find-file filename)
		(message "File '%s' isn't readable" filename))))))
   
)


;; ======================================================================
;; Support for .subckt search !?
;; ======================================================================

;; What about searching from an included file, how to find the
;; top-level then ?  think I've cracked it: .end identifies top-level
;; spice files so remember the last one to start search from if the
;; search fails

;; BUG: doesn't handle nested .subckt defs ! Reports first found match...

(defun eldo-search-included-files (subckt)
  (save-excursion
    (let ((mrk nil))
      (goto-char (point-min))
      (while (and
	      (search-forward-regexp eldo-library-regexp 
				     (point-max) t)
	      (not mrk))
	(beginning-of-line)
	(if (looking-at (concat eldo-library-regexp-start 
				eldo-library-regexp-end))
	    (progn
	      (setq filename (expand-file-name
			      (substitute-in-file-name (match-string 2))))
	      ;;(message "filename is %s" filename)
	      (if (file-readable-p filename)
		(setq mrk 
		      (eldo-search-file-for-subckt filename subckt))
	      (message "File '%s' isn't readable" filename))))
	(end-of-line)) ; while
      mrk) ; let
    ) ; save-
  )


(defvar eldo-subckt-search-master-filename nil
  "latest top-level (identified by .end in file) .cir file used in
subcircuit searches.")

(defun eldo-search-file-for-subckt (filename subckt)
  "Searches a file for a .subckt definition. Remembers
`eldo-subckt-search-master-filename' for future subckt searches."
  (save-excursion
    (set-buffer (find-file-noselect filename))
    (condition-case nil
	(let ((index-alist (imenu--make-index-alist t))
	      (mrk nil))
	  (if (assoc "*End*" index-alist)
	      (setq eldo-subckt-search-master-filename buffer-file-name))
	  (setq mrk (assoc-ignore-case subckt index-alist))
	  (if mrk mrk
	    (eldo-search-included-files subckt))
	  )
      (error nil))))


;; History of subckt searches.
(defvar eldo-subckt-search-history nil
  "History of subcircuit searches.")

(defun eldo-guess-subckt-name ()
  "guesses name of subckt from context, multiple lines"
  (let ((subckt "")) ; (current-word)
    (save-excursion
      (beginning-of-line)
      (while (and (looking-at "^+")
		  (not (forward-line -1))))
      (if (looking-at eldo-xinstance-regexp)
	  (progn
	    (message "Could it be '%s' ?" (match-string 5))
	    (setq subckt (match-string 5))
	    (remove-text-properties 0 (length subckt) '(face nil) subckt)
	    ))) ; save-
    subckt))


(defun eldo-visit-subckt-def (mrk)
  "Helper function visiting buffer and mark specified."
  (if (eq (marker-buffer (cdr mrk))
	  (current-buffer))
      (if (or
	   eldo-running-xemacs ; then push mark always
	   (not (and transient-mark-mode mark-active))) ; emacs, check if active region
	  (push-mark)))
  (pop-to-buffer (marker-buffer (cdr mrk)) t)
  (widen)
  (goto-char (cdr mrk)))


(defun eldo-search-subckt (subckt-args)
  "Searches for the .subckt definition with name under cursor, or any other 
name specified by user. Be CAREFUL using this command. Depending on the 
structure of your spice decks this might find wrong definitions. To AVOID any
such problems always start searching from the TOP-LEVEL spice deck (ie. the
file that is supplied to the simulator). If you start searching from an
included file, potentially the definition is not found or it is found starting
from ANOTHER top-level file (which could result in a completely wrong
search result). 
This search command places the mark if search result is in the same file, 
return to the search start position by using C-u C-<SPC> or C-u C-@."
  (interactive
   (list (let* ((default-subckt (eldo-guess-subckt-name))
		(input (read-from-minibuffer
			"Subcircuit name: "
			default-subckt nil nil 
			eldo-subckt-search-history)))
	   (if (string= input "")
	       (if (string= default-subckt "")
		   (error "No subckt args given")
		 default-subckt)
	     input))))
  ;(message (format "name of subckt is %s" subckt-args))
  (let (mrk)
    (setq mrk (eldo-search-file-for-subckt buffer-file-name subckt-args))
    ; (message (format "mark is %s" (cdr mrk)))
    (if (and (cdr mrk) (markerp (cdr mrk)))
	(eldo-visit-subckt-def mrk)
      (progn
	(message "Couldn't find subcircuit '%s', retrying search in top-level file" 
		 subckt-args))) ; if
    (if (and (not mrk)
	     eldo-subckt-search-master-filename)
	(progn
	  (setq mrk (eldo-search-file-for-subckt 
		     eldo-subckt-search-master-filename subckt-args))
	  (if (and (cdr mrk) (markerp (cdr mrk)))
	      (progn
		(eldo-visit-subckt-def mrk)
		(message "Used top-level file '%s' to find '%s'" 
			 eldo-subckt-search-master-filename subckt-args)
		)
	    (progn
	      (message 
	       (format "Couldn't find subcircuit '%s', retry search in top-level file" 
		       subckt-args))
	      ))))))




;; ======================================================================
;; loading of include files of current deck.


(defun eldo-load-include-files ()
  "Loads all files that are included in this deck. Makes it more easy
to load a project. This loading is not recursive. Files already
loaded are not reloaded or scanned for .includes. This function is
only guaranteed to work when all included files are not already loaded."
  (interactive)
  (eldo-load-include-files-recursively 't)
)

(defun eldo-load-include-files-recursively (&optional non-recursive)
  "Loads all files that are included in this deck. Makes it more easy
to load a project. This loading can occur recursively. Files already
loaded are not reloaded or scanned for .includes. This function is
only guaranteed to work when all included files are not already loaded."
  (interactive)
  (let ((index-alist (imenu--make-index-alist t))
        l filename)
    (if (setq l (cdr (assoc "Libraries" index-alist))) ;;file contains include files/libraries
        (while l
          (setq filename (expand-file-name
                          (substitute-in-file-name (car (car l)))))
          ;;(message "Trying to load %s" filename)
          (if (and (file-readable-p filename)
                   (not
                    (assoc filename ;; already loaded
                           (mapcar
                            (lambda (buffer)
                              (cons (buffer-file-name buffer) buffer))
                            (buffer-list)))))
              (save-excursion
                ;; (message "filename is %s" filename)
                (set-buffer (find-file-noselect filename))
                (unless non-recursive
                    (eldo-load-include-files-recursively))))
          (setq l (cdr l))))))

;; ======================================================================
;; unloading of eldo files except current deck.

(defun eldo-unload-other-decks ()
  "Kills all other eldo files except current one. Makes it easy to
unload a lot of eldo files without restarting emacs."
  (interactive)
  (save-excursion
    (let ((current (current-buffer)))
      (mapcar
       (lambda (buffer)
	 (set-buffer buffer)
	 (if (and (eq major-mode 'eldo-mode)
		  (not (eq current buffer)))
	     (progn
	       (message "Killing %s" buffer)
	       (kill-buffer buffer))))
       (buffer-list)))))


;; ======================================================================
;; folding for commented out regions ...
;;; taken from and adapted:
;;; Filename: foldingo.el
;;; Author: Christian Queinnec <Christian.Queinnec@lip6.fr>
;; This is work in progress; (user) interface might change

(defvar eldo-some-comment-regions-are-hidden nil
  "Var that keeps track of hiding of comment regions.")
(make-variable-buffer-local 'eldo-some-comment-regions-are-hidden)

(defvar eldo-last-hide-comment-regions-tick nil
  "Var that keeps track when last time comment hiding was called.")
(make-variable-buffer-local 'eldo-last-hide-comment-regions-tick)


(defun eldo-hide (from to)
  "Not supported, don't use this."
  (interactive "*r")
  (eldo-hide-region from to t))

(defun eldo-unhide (from to)
  "Not supported, don't use this."
  (interactive "*r")
  (eldo-hide-region from to nil))

(defun eldo-hide-all-comments-p ()
  "Checks if there are comments that can be hidden...; assumes that if
the user edited the file, new comments might have been created and
thus hide all comments must be activated. This is not the best
criterion, but it is safe."
;  (interactive)
;  (message "buffer tick is %s, last hide comment is %s"
;	   (buffer-modified-tick) eldo-last-hide-comment-regions-tick)
  (if (and 
       eldo-last-hide-comment-regions-tick ; can be nil
       (= eldo-last-hide-comment-regions-tick (buffer-modified-tick)))
      nil t))

(defun eldo-hide-all-comments ()
  "Hides all commented out regions in the current eldo deck. Allows
to get a better overview of the deck if many lines are commented out.
It relies on having all regions being commented out using the
\"Comment Region\" menu entry that uses `comment-region'. To avoid
problems, make sure documentation is not part of comments. For
instance by using the doc starters available in the eldo languages,
or by making sure they are different from the comment lines generated
with the menu entry, by placing two *'s when the default comment is
only one *. If you want to unhide all the hidden comment lines, use
`eldo-unhide-all-comments'."
  (interactive)
  (eldo-unhide-all-comments)
  (setq eldo-last-hide-comment-regions-tick (buffer-modified-tick))
  (save-excursion
    (goto-char (point-min))
;    (message "beginning of buffer reached %s" (point))
    (forward-line)
    (while (search-forward-regexp (concat "^" comment-start)
				  (point-max) t)
      (beginning-of-line)
;      (message "reached %s" (point))
      (let ((beg (point))
	    end
	    lines)
;	(message "found start of regexp %s" (point))
	(setq lines (forward-line))
	; (message "%s left" lines)
	;; the empty lines following commented lines are also hidden
	(while (and (looking-at (concat "^\\(" comment-start "\\|[ ]*$\\)"))
		    (= (setq lines (forward-line)) 0))
	  ; (message "%s left" lines)
	  )
	(setq end (point))
	(eldo-hide-region beg end t))))
)

(defun eldo-unhide-all-comments ()
  "Unhides all hidden comment regions."
  (interactive)
  (setq eldo-some-comment-regions-are-hidden nil)
  (setq eldo-last-hide-comment-regions-tick nil)
  (eldo-hide-region (point-min) (point-max) nil)
)

(defun eldo-hide-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char from)
;      (foldingo-discard-overlays (point) to 'invisible 'eldo-comment)
      (eldo-discard-overlays from to 'eldo-comment)
      (if flag
	  (let ((overlay (make-overlay (point) to)))
	    (eldo-make-overlay-hidden overlay))))))

(defun eldo-make-overlay-hidden (overlay)
  ;; Make overlay hidden and intangible.
;  (overlay-put overlay 'intangible t)
  (overlay-put overlay 'invisible 'eldo-mode)
  (overlay-put overlay 'eldo-comment t)
  (setq eldo-some-comment-regions-are-hidden t)
;;  (overlay-put overlay 'intangible t)
)

(defun eldo-discard-overlays (from to prop)
  "discards overlays in region FROM to TO that have property PROP set."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char from)
      (while (< (point) to)
	(let ((overlays (overlays-at (point))))
	  (while overlays
	    (let ((o (car overlays)))
	      (if (overlay-get o prop)
		  (delete-overlay o)))
	    (setq overlays (cdr overlays))))
	(goto-char (next-overlay-change (point)))))))



;;------------------------------------------------------------
;; Index menu (using `imenu.el') to create an index of subckt/models
;;------------------------------------------------------------

(defconst eldo-misc-model-names
  '(
    "res" "r" "cap" "ind"   
    "njf" "pjf" "nsw" "psw"
    "opa" "modfas" "logic" "a2d" "d2a"
    "macro"
    )
  "List of misc Eldo models types (no transistors)")


(defvar eldo-imenu-generic-expression
  (list
   '("*End*"
     "^\\.\\(end\\)\\>"
   1)
   (list
    "Misc"   
    (concat  
     "^\\s-*\\.model\\s-+"  eldo-model-name eldo-line-break 
     "\\s-+\\("
     (regexp-opt eldo-misc-model-names)   
     "\\)\\>" )
    1)
   (list 
    "Libraries"   
    (concat eldo-library-regexp-start
	    eldo-library-regexp-end)
    2)
   (list
    "Diodes"
    (concat "^\\s-*\\.model\\s-+"   
	    eldo-model-name  eldo-line-break "\\s-+d\\>")   
    1)
   (list
    "Bipolars"   
    (concat "^\\s-*\\.model\\s-+"   
	    eldo-model-name  eldo-line-break "\\s-+\\(npn\\|pnp\\)\\>")   
    1)
   (list
    "Mosfets"   
    (concat "^\\s-*\\.model\\s-+"   
	    eldo-model-name  eldo-line-break "\\s-+\\(n\\|p\\)mos\\>")   
    1)
   (list
    "Analysis"  
    (concat "^\\s-*\\.\\(" 
	    (regexp-opt eldo-analysis-keywords) 
	    "\\)\\>")
    1)
   (list 
    "Sections"
    (concat eldo-section-regexp-start "\\("
	    (regexp-opt eldo-section-headings) "\\)\\(.*\\)$")
    1)
;;   '("Subckts" 
   (list nil
     "^\\s-*\\.subckt\\s-+\\([a-z]\\w+\\)"   
     1)
  )
"Imenu generic expression for Eldo Mode.  See `imenu-generic-expression'."
)


(defun eldo-imenu-init ()
  "Initialize index menu."
  (set (make-local-variable 'imenu-generic-expression)
       eldo-imenu-generic-expression)
  (set (make-local-variable 'imenu-case-fold-search) t)    
  (imenu-add-to-menubar "Index")
  )

;;------------------------------------------------------------
;; Hacks to implement the find function menu bar for eldo subckts/models
;; Fortunately eldo only provides one means of abstraction so the parsing is
;; very easy.   
;;------------------------------------------------------------


(when (fboundp 'function-menu)
  (require 'func-menu)

  (defconst fume-function-name-regexp-eldo
    "^\\s-*\\.\\(subckt\\|model\\)\\s-+\\([a-z]\\w+\\)"
    "Expression to parse Eldo subcircuit names.")

  (defun fume-find-next-eldo-function-name (buffer)
    "Searches for the next eldo subcircuit name in BUFFER."
    (set-buffer buffer)
    (setq case-fold-search 't)		;;otherwise func-menu bombs....
    (if (re-search-forward fume-function-name-regexp nil t)
	(let ((beg (match-beginning 2))
	      (end (match-end 2)))
	  (cons (buffer-substring beg end) beg))))
  )    

(defun eldo-func-menu-init ()
  "Initialize function menu."

  ;; hook in the eldo mode regular expression above into the association list of
  ;; regexps used by the function menu generator
  (setq fume-function-name-regexp-alist
	(purecopy
	 (append
	  fume-function-name-regexp-alist
	  (list
	   '(eldo-mode . fume-function-name-regexp-eldo)))))
    
    
  ;; hook in the search method above into the association list used by the
  ;; function menu generating code
  (setq fume-find-function-name-method-alist
	(purecopy
	 (append
	  fume-find-function-name-method-alist
	  (list '(eldo-mode . fume-find-next-eldo-function-name)))))
  ;; Now activate func-menu - I hope that these settings don't
  ;; interfere with users settings    
  (make-local-variable 'fume-menubar-menu-name)
  (make-local-variable 'fume-buffer-name)
  (make-local-variable 'fume-index-method)
  (setq fume-menubar-menu-name "Subckts"
	fume-buffer-name "*Subcircuits List*"
	fume-index-method 2
	)
  (make-local-hook 'find-file-hooks)
  (add-hook 'find-file-hooks 'fume-add-menubar-entry)    
  (define-key global-map '(shift button2) 'fume-mouse-function-goto)
  (fume-add-menubar-entry)
  )


;;------------------------------------------------------------
;; speedbar stuff
;;------------------------------------------------------------


(defun eldo-speedbar-initialize ()
  "Initialize speedbar."
  ;; general settings
  ;; ELDO file extensions (extracted from `auto-mode-alist')
  (let ((mode-alist auto-mode-alist))
    (while mode-alist
      (when (eq (cdr (car mode-alist)) 'eldo-mode)
	(speedbar-add-supported-extension (car (car mode-alist))))
      (setq mode-alist (cdr mode-alist))))
  )

(defun eldo-speedbar (&optional arg)
  "Open/close speedbar."
  (interactive)
  (if (not (fboundp 'speedbar))
      (error "WARNING:  Speedbar is only available in newer Emacs versions")
    (condition-case ()			; due to bug in `speedbar-el' v0.7.2a
	(speedbar-frame-mode arg)
      (error (error "WARNING:  Install included `speedbar.el' patch first")))))



;;------------------------------------------------------------
;; Support for compilation (simulation) - doesn't work currently
;;------------------------------------------------------------

(defvar eldo-simulation-font-lock-keywords
  (list
   '("TOTAL POWER DISSIPATION" . font-lock-keyword-face)
   '("Performing .* analysis" . font-lock-keyword-face)
   '("\\<\\w+ = \\w+\\>" . font-lock-keyword-face)
   )
  "Additional expressions to highlight in Compilation mode.")


(defun eldo-simulation-buffer-name-function (arg)
  (concat "*Eldo-simulation-" (buffer-name) "*")
  )

;; compile wrapper
(defun eldo-compile ()
  "eldo wrapper function for compile."
  (interactive)
  (unless buffer-file-name
    (error 
     "You have to save buffer into a file before running a simulator on it"))
  (setq compile-command 
	(concat 
	 "eldo "
	 (file-name-nondirectory buffer-file-name)
	 " " eldo-simulator-switches))
  (call-interactively 'compile nil))


;;------------------------------------------------------------
;; Eldo-mode starts here
;;------------------------------------------------------------
;;;###autoload
(defun eldo-mode ()
  "Eldo-mode is a major mode for highlighting ELDO netlist files.
Eldo is the analog simulator of Mentor Graphics.
This mode requires font-lock,easy-menu and several other modes that
are part of (x)emacs since ages.
Xemacs-21 is well supported. For Emacs or older versions of Xemacs, your
mileage may vary.
Turning on Eldo mode calls the value of the variable `eldo-mode-hook'   
with no args, if that value is non-nil.

- COMMENTS:
Two types of comments are used:
- the '*' symbol is used to comment out a whole line - that symbol has
to be at the beginning of a line   
- the '!' symbol is used to document your netlist   
 
There is also comment hiding support: you can hide all commented out regions 
in a buffer to improve readability. When parts of the deck are hidden the 
string \"H+\" appears in the modeline.


- DEFAULT TEMPLATE:
When you create a new file, a default template is inserted in it. You can
create your own template simply by putting it in your home directory, in a
file called `.eldo' .

- TEMPLATES FOR MODELS AND MACROMODELS:
A lot of templates are available. They are all listed in the 'Eldo' menu 
('Add Element' 'Macros' and 'Extract' submenus). 
You can also use them simply by hitting the <TAB> key after the appropriate tag.
For example, type 'M<tab>' at the beginning of a line and you will be prompted 
with a complete Mosfet template. Most tags are pretty straightforward i.e 'C' 
for a capacitor, 'L' for an inductance etc...
You can type `C-h v tempo-tags'for a complete list of tags and associated templates.
 
Note: to insert a real <TAB>, use <shift-TAB>.

- KEY BINDINGS:
\\{eldo-mode-map}


- HIGHLIGHTING (fontification):
By default, eldo-mode uses its own  set of faces for font-lock. You
can modify these by using the 'customize' package (group: eldo-mode) .
Eldo-mode will then use a custom set of font-lock faces to fontify your file.
Libraries are a special case: library names will be highlighted, and you can  
load them using shift/middle mouse button.

- COMPILATION (simulation)
You can use the 'compile' button to start Eldo with the current buffer as
input netlist. This functionality is also accessible through the menu.

- STROKES (xemacs>=20 only)
Since Eldo is Mentor Graphics' simulator, it would seems quite logical to include
support for strokes in `eldo-mode'. However this kind of feature doesn't belong
in a major mode, so I leave it to the user to configure his own strokes.

- FUNC-MENU/IMENU/SPEEDBAR
Support for both func-menu and imenu are available in eldo-mode, imenu is activated
by default if it is available (this can be overridden with the 'customize package).
This allows you to have a list of subcircuits and models in a menu.   
Speedbar functionalities are also provided together with imenu support.
Note that Imenu is needed for automatic loading of 'included' and library files.  

- SEARCH FOR .SUBCKT DEFS: `eldo-search-subckt' or `C-c C-s'
       + extracts subcircuit name from context
       + search history
       + mark is set where search has been started if the definition is found
         in the same file. Return to mark with `C-u C-<SPC>' (or `C-u C-@')
         as with interactive searches (fi `isearch-forward')
       + be careful when starting the search from an included file,
         correctness can not be guaranteed. Starting a search from 
         a top-level .cir file gives correct results. The latest used
         top-level file is stored (a top-level file contains a .end 
         statement !), and also searched if the subckt def is not found in
         a first pass (for instance when starting from an included file).

- CUSTOMIZATION
Eldo-mode is customizable through the (X)emacs 'customize' packages.
Customizable features are faces for font-locking, section headers, Imenu/func-menu
activation and switches to pass to eldo as a compiler.

 
To add automatic support put something like the following in your .emacs file:
  \(autoload 'eldo-mode \"eldo-mode\" nil  t\)
  \(setq auto-mode-alist \(cons '\(\"\\\\.cir\" . eldo-mode\) \
auto-mode-alist\)\)
  \(setq auto-mode-alist \(cons '\(\"\\\\.ckt\" . eldo-mode\) \
auto-mode-alist\)\)    

This will trigger eldo-mode on all files that end in '.cir' or '.ckt' .
If your netlists file don't have a particular extension like '.cir'
you can try adding the following lines at the end of the file:

*** Local Variables:
*** mode:eldo
*** End:

This should trigger eldo-mode on that particular file when opened
(but don't forget to put:
    \(autoload 'eldo-mode \"eldo-mode\" nil  t\)
in your .emacs)

Copyright © 1997-2002 Emmanuel Rouat <emmanuel.rouat@wanadoo.fr>"

  (interactive)
  (kill-all-local-variables)
  (setq tempo-interactive t)
  (setq mode-name "Eldo")
  (setq major-mode 'eldo-mode)   
  (use-local-map eldo-mode-map)
  (setq case-fold-search 't)

  ;; support for auto-fill:
  (set (make-local-variable 'fill-prefix) "+ ")
  ;;(set (make-local-variable 'auto-fill-inhibit-regexp) "^\*[^\.\+].*")
  (set (make-local-variable 'auto-fill-inhibit-regexp) ".* !.* ")   

  ;; support for paragraphs (is it useful?)
  (set (make-local-variable 'paragraph-start) "^!-.*$")	; this is really arbitrary
  (set (make-local-variable 'paragraph-separate) "^!-.*$")

  ;; Tempo tags - using 'tempo-local-tags' doesn't work (why??)
  (set (make-local-variable 'tempo-tags) (append eldo-tempo-tags tempo-tags))

  ;; syntax table table
  (set-syntax-table eldo-mode-syntax-table)

  ;; support for comments
  (set (make-local-variable 'comment-start) "*")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*\\*") ; is buffer local   
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'comment-multi-line) nil)

   
  ;; use Eldo as compiler, on current buffer
  (set (make-local-variable  'compile-command) nil) ;; set in eldo-command
  (set (make-local-variable  'compilation-read-command) 'nil)
  (set (make-local-variable  'compilation-buffer-name-function)
       'eldo-simulation-buffer-name-function)
  (set (make-local-variable 'compilation-error-regexp-alist) nil)
  (set (make-local-variable 'compilation-font-lock-keywords) nil)
  (set (make-local-variable 'compilation-file-regexp-alist) nil)
  (setq compilation-font-lock-keywords 'eldo-simulation-font-lock-keywords)


  ;; add Eldo mode menu
  (if (not eldo-menu-list)
      (setq eldo-menu-list (eldo-create-mode-menu)))
  (easy-menu-add eldo-menu-list)	; for XEmacs
  (easy-menu-define eldo-menu eldo-mode-map
		    "Menu keymap for Eldo Mode." eldo-menu-list)

  ;; initialize default header for this buffer
  (set (make-local-variable 'eldo-default-header)   
       (concat   
	"!---------------------------------------------------------------\n"
	"!-- Project       : \n"
	"!-- Circuit name  : " (buffer-name) "\n"
	"!---------------------------------------------------------------\n"
	"!-- Designer(s)   : " (user-full-name) " <" user-mail-address ">\n"
	"!-- Library       : \n"
	"!-- Purpose       : \n"
	"!-- Inputs        : \n"
	"!-- Outputs       : \n"
	"!-- Supplies      : \n"
	"!-- References    : \n"
	"!---------------------------------------------------------------\n"
	)   
       )
  ;; if new file add a default template
  (if (= (buffer-size) 0)
      (eldo-file-initialize))
    
  ;; initialize imenu/func-menu
  (if eldo-use-imenu
      (if (fboundp 'imenu)
	  (progn   
	    (eldo-imenu-init)
	    (eldo-speedbar-initialize))))

  (if eldo-use-func-menu
      (if (fboundp 'function-menu)
	  (eldo-func-menu-init)))

  ;; font lock start-up
  (set (make-local-variable 'font-lock-defaults)
       (list 'eldo-font-lock-keywords nil t)) ;; nil -> t, don't do strings

  ; now hook in 'eldo-mode-colorize-libraries  

  (make-local-hook 'font-lock-mode-hook)
  (make-local-hook 'font-lock-after-fontify-buffer-hook); doesn't exist in emacs 20
  (add-hook 'font-lock-mode-hook 'eldo-colorize-libraries-buffer t t)
  (add-hook 'font-lock-after-fontify-buffer-hook 'eldo-colorize-libraries-buffer t t) ; not in emacs 20
  (make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions 'eldo-colorize-libraries t t)

  ;; now run eldo-mode-hook
  (run-hooks 'eldo-mode-hook)

  )

(provide 'eldo-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGELOG ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Created:       June 97 (0.1)
;; Modified: September 97 (0.2)   
;;           5 October 97 (0.3) - templates added
;;         20 November 97 (0.4) - comment-uncomment defun
;;         21 November 97 (0.5) - font names changed --> eldo-**-face   
;;                                changed comment definition and regexps
;;               7 May 98 (0.6) - modifications in fonts definitions
;;                                (removed 'set-face-font ..' lines)
;;                                modified 'plot' regexp (added probe)
;;              13 May 98 (0.7) - created 'waveforms' submenu
;;                                added 'macromodels' submenu
;;                                and some macros templates
;;                                Corrected bug in comment regexp
;;              14 May 98 (0.8) - regexps simplified - fontification
;;                                seems faster now (?)
;;              6 July 98 (0.9) - small bug in assignment regexp
;;          29 August 98 (0.91) - added functions support
;;                                (copy-paste from spice-mode by C.J. Vieri)
;;        2 September 98 (0.92) - small bug fixes
;;       17 September 98 (0.93) - added 'extracts' templates
;;         23 October 98 (0.94) - added some 'extracts' templates
;;         30 October 98 (0.95) - added 'macro' templates
;;         28 January 99 (0.96) - added labels in 'extracts'
;;                                removed some buggy defmacs
;;            23 July 99 (0.97) - fixed buggy syntax table
;;          12 August 99 (0.98) - added 'changelog' support
;;                                added automatic template at startup
;;                                added abbrevs and auto-fill
;;                                expanded menu
;;        26 August 1999 (0.99) - fixed bug in file initialization
;;                                made dates Y2K compliant 8-)
;;                                added voltage/current source templates
;;                                added bunch of macromodels templates
;;                                modified default template and sections   
;;                                fixed bug in changelog entry   
;;       3 January 2000 (0.991) - fixed bug in fontification (order of
;;                                fontification is crucial)
;;                                added automatic loading of func-menu mode
;;      12 January 2000 (0.992) - auto-fill-mode not turned on automatically
;;                                (leave it to user)
;;                                default font-lock faces may be used now
;;                                simplified a few regexps
;;                                and made a very complicated one!! (assignments)
;;                                corrected syntax table
;;                                replaced '=' by '-' in comment-bar (recent versions   
;;                                of eldo choke on '!=')
;;      18 January 2000 (0.993) - added automatic loading of libraries on   
;;                                mouse-click
;;                                fixed bug with function-menu support
;;      24 January 2000 (0.994) - added support for paragraphs (useful??)
;;                                fixed bug in syntax table (#c #e comments)
;;      25 January 2000 (0.995) - fixed last bug with func-menu support (at last!!)
;;                                removed 'pos' variable (use 'let' instead)
;;                                made font-lock mode hooks local   
;;                                made default value 'eldo-use-default-colors' true
;;        26 March 2000 (0.996) - added key bindings
;;                                made a few changes in colors
;;                                misc changes
;;                                exp. support for subckt names fontification
;;          26 May 2000 (0.997) - bugfix in support for subckt names fontification
;;         18 June 2000 (0.998) - added support for imenu
;;                                multi-line subckt fontification seems to work now
;;                                changed handling of comments   
;;    14 September 2000 (0.999) - fixed multi-line subckt fontification with regexp
;;                                by Geert A. M. Van der Plas (works great! yahooo!)
;;                                added support for customization (group eldo-mode)
;;                                now use 'defface' for faces definition
;;                                added 'eldo-reformat' function
;;                                loading of libraries: now takes into account 'key=..'
;;                                use easymenu for menus now
;;                                looks like imenu support is fixed now
;;                                lots of cosmetic changes
;;                                added 'eldo-customize' function (from spice-mode)
;;                                now try to use usual font-lock faces, plus a few
;;                                 specific ones
;;   27 September 2000 (1.0-pre1)-release of 1.0-pre1
;;    07 November 2000 (1.0-pre2)-fixed bug in diodes imenu regexp
;;                                added multi-line support for imenu
;;                                added some vhdl-mode like key bindings
;;       05 April 2001 (1.0-pre3)-fixed loading of libraries using Geerts code
;;                                (use overlays instead of extents)
;;                                fixed eldo-model-name regexp
;;                                added eldo-mode-simulator-switches as defcustom
;;       10 April 2001 (1.0-pre4)-fixed loading of libraries using Geerts code
;;                                modified empty file initialization
;;                                some clean up in the file
;;       18 April 2001 (1.0-pre5)-implemented Geerts' sections support code
;;                                made function/variables naming consistent with the
;;                                'Emacs Lisp Coding Conventions'
;;                                fixed multi-line subckt fontification (AGAIN!)
;;                                corrected misspellings
;;               2 May 2001 (1.0)-release of 1.0
;;              21 May 2001 (1.1)-fixed regexp eldo-library-regexp-end
;;                                added 'ST' menu for manipulation of ST DK netlists
;;                                added 'settling' macro
;;                                added 'NETLIST' section header
;;                                added 'four' as analysis keyword     
;;           April 2002 (1.2-pre)-added loading of 'include/lib' files (from spice-mode)
;;                                aligned imenu on spice-mode
;;                                aligned eldo-mode menu on spice-mode (more or less)
;;                                updated 'eldo-load-file-at-mouse' substitute function
;;                                updated templates to catch up on spice-mode
;;                                updated lots of macromodel templates
;;                                added 'hide comments' util (from spice-mode)
;;          April 2002 (1.2-pre3)-added loading of 'include/lib' files (from spice-mode)
;;                                modified 'gain margin' extract macro
;;                                added Geerts 'eldo-compile' function
;;                                changed 'library-regexp-start' for Emacs (G.vdP)
;;          April 2002 (1.2-pre4)-added Geerts 'search subckt def' feature (but made it
;;                                case insensitive)
;;                                took Geerts 'xinstance-regexp' - now fontifies Subckt/Model
;;                                names even if there is a commented out line in between
;;          April 2002 (1.2-pre5)-Test release
;;            15 April 2002 (1.2)-Release of 1.2
;;          16 April 2002 (1.2.1)-aligned eldo-mode fontification on spice-mode regarding
;;                                color of non '.' keywords starting line
;;               May 2002 (1.2.2)-updated 'satr' macromodel entry
;;                                corrected bug in 'settling' macro
;;            August 2002 (1.2.3)-added digital macromodel definitions
;;                                added mixed-signal macromodel definitions
;;                                removed abbrev-table definition
;;                                added 'ends' as command keyword (GvdP)
;;         September 2002 (1.2.4)-added switched cap macromodel definitions
;;                                added macromodel names ('Y' instances) fontification
;;                                made tempo-tags buffer-local (eldo-tempo-tags)      
;;          February 2003 (1.2.5)-put 'checksoa' and 'conso' in analysis keywords
;;                                added 'nonoise' as keyword
;;                                added some sst keywords (eldo-RF)
;;                                improved doc-starter regexp (now handles global nets like 
;;                                'VDD!' correctly
;;
;;   
;;; eldo-mode.el ends here

;;; Local Variables:
;;; mode:Emacs-lisp
;;; End:   

