;;; face-lock.el --- general colour/color/grayshade/font-lock fontification.

;; Copyright (C) 1994 Simon Marshall.

;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; Keywords: faces
;; Version: 3.03

;; LCD Archive Entry:
;; face-lock|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Colour/color/grayshade/font fontification for font-lock mode.|
;; 18-Jul-94|3.03|~/misc/face-lock.el.Z|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;;
;; To make `font-lock-mode' faces use colours or grayscale as well as fonts and
;; underlining, and also provide easy (interactive) modification of faces.
;;
;; Face-lock works with monochrome, grayscale and colour displays, and will run
;; under Lucid Emacs too.
;;
;; See also the fast-lock package.

;; Note that:
;;
;; - In Emacs 19.25 and below, the attributeUnderline resource, if given, is
;;   interpreted as "on" whatever the resource value.
;; - If you run fast-lock, you will find that your existing text properties
;;   caches do nothing.  Just delete the text properties cache files and let
;;   fast-lock write new ones.

;; Installation:
;; 
;; Put this file somewhere where Emacs can find it (i.e., in one of the paths
;; in your `load-path'), `byte-compile-file' it, and put in your ~/.emacs:
;;
;; (if (featurep 'font-lock)
;;     (require 'face-lock)
;;   (eval-after-load "font-lock" '(load "face-lock")))
;;
;; Lucid Emacs users (should try Emacs---with Lucid you) will have to put:
;;
;; (if (featurep 'font-lock)
;;     (require 'face-lock)
;;   (add-hook 'font-lock-mode-hook '(lambda () (require 'face-lock))))
;;
;; Start up a new Emacs and use font-lock as usual.
;;
;; If Emacs appears to be using unsuitable colours (or lack of), face-lock may
;; have incorrectly guessed the type of display or the brightness of the frame
;; background.  Look at the variables `face-lock-display-type' and
;; `face-lock-background-mode'.  If you wish to change the face attributes
;; themselves, look at the variable `face-lock-face-attributes'.  To change the
;; default attributes, set these variables in your ~/.emacs or ~/.Xdefaults.
;; To set them interactively, look at the function `modify-face'.
;;
;; (An alternative method of installation, if you understand how this software
;;  works, is just to put in your ~/.emacs:
;;
;;  (autoload 'face-lock-mode "face-lock" "Toggle Face Lock mode." t)
;;  (autoload 'face-lock-facify-buffer "face-lock"
;;    "Facify the current buffer the way `face-lock-mode' would." t)
;;
;;  and use `face-lock-mode' instead of `font-lock-mode' etc. in your ~/.emacs
;;  or interactively.  Of course, you still refer to `font-lock-keywords' and
;;  the like by their usual names.  However, this method of installation may be
;;  further confusing since `font-lock-mode' will use colours too.
;;
;;  Turning on face-lock automatically for any particular mode is made slightly
;;  easier by the function `turn-on-face-lock'.  Put in your ~/.emacs:
;;
;;  (autoload 'turn-on-face-lock "face-lock"
;;    "Unconditionally turn on Face Lock mode.")
;;
;;  (add-hook 'c-mode-hook 'turn-on-face-lock)
;;  (add-hook 'emacs-lisp-mode-hook 'turn-on-face-lock)
;;
;;  though the above methods of use mean that your code will refer to face-lock
;;  in many places, with the obvious consequences if you move to an environment
;;  that---for some bizarre reason---does not have face-lock.)

;; Feedback:
;;
;; Please note (before complaining;-) that my X resources include:
;; Emacs.background:	#fffff0
;; Emacs.font:		8x13bold
;;
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.
;;     - Simon Marshall (Simon.Marshall@mail.esrin.esa.it)

;; History:
;;
;; From Lucid's font-lock.el: It's called font-lock-mode here because on the
;; Lispms it was called "Electric Font Lock Mode."  It was called that because
;; there was an older mode called "Electric Caps Lock Mode" which had the
;; function of causing all of your source code to be in upper case except for
;; strings and comments, without you having to blip the caps lock key by hand
;; all the time (thus the "electric", as in `electric-c-brace'.)
;;
;; 1.00--1.01:
;; - Changed structure of `color-lock-faces'.
;; - Added `color-lock-background-is-light' and `color-lock-face-initialize'.
;; - Made defaults for dark backgrounds.  No guarantees on tastefulness, mind.
;; - Modifies `minor-mode-alist'.
;; - Uses the colours' names for face names.  Yes, I've looked at hilit19.
;; 1.01--2.00: font-lock + color-lock = face-lock.
;; - Changed name to face-lock and added non-colour attributes.
;; - Changed structure of `face-lock-faces' for fonts and monochrome.
;; - Changed `color-lock-background-is-light' to `face-lock-background-mode'.
;; - Changed `color-lock-face-initialize' to `face-lock-set-face'.
;; - Uses generic face-lock face names for font-lock face names.
;; 2.00--2.01:
;; - Uses `frame-parameters' for `face-lock-background-mode' initialisation.
;; 2.01--2.02:
;; - Uses `face-try-color-list' to set foreground in `face-lock-set-face'.
;; 2.02--2.03:
;; - Fix for Lucid: Use `x-display-planes' for `x-display-color-p'.
;; - Fix for Lucid: Made `set-face-bold-p' for `make-face-[un]bold'.
;; - Fix for Lucid: Made `set-face-italic-p' for `make-face-[un]italic'.
;; - Implemented `face-try-color-list' for those without.
;; 2.03--2.04: now works with Lucid Emacs, thanks to Giacomo Boffi's patience.
;; - Fix for Lucid: Use `screen-parameters' for `frame-parameters'.
;; - Fix for Lucid: Suggest `add-hook' for `eval-after-load'.
;; - Package now provides itself.
;; 2.04--2.05:
;; - Fix for Lucid: Use `x-color-display-p' for `x-display-color-p'.
;; - Added `turn-on-face-lock' for easy `*-mode-hook' use.
;; - Added `face-lock-display-type' and `x-display-grayscale-p' for display.
;; - Changed `face-lock-background-mode' to describe brightness only.
;; - Changed structure of `face-lock-faces' for grayscale.
;; - Added `face-lock-modify-face' for changing `face-lock-faces'.
;; 2.05--2.06:
;; - Removed `face-lock-modify-face'.
;; - Added `face-lock-set-faces' for applying `face-lock-faces'.
;; 2.06--2.07:
;; - Fix for Emacs: Use unary in `x-display-planes' in `x-display-grayscale-p'.
;; 2.07--3.00:
;; - Fix for Lucid: Use `ignore' for `x-defined-colors'.
;; - Changed `face-lock-faces' to `face-lock-face-attributes'.
;; - Changed structure of `face-lock-face-attributes'.
;; - Changed `face-lock-*-face' face names to `font-lock-*-face'.
;; - Removed `face-lock-set-faces'.
;; - Changed `face-lock-set-face' to `modify-face'.
;; 3.00--3.01: really works with Lucid, thanks to Matthew Liggett's patience.
;; - Fix for Lucid/bold/italic resource use in face installation.
;; - Accepts `monochrome'/`greyscale' as well as `mono'/`grayscale'.
;; 3.01--3.02: release to beta test.
;; 3.02--3.03:
;; - Added `face-lock-make-face' for face initialisation.

(require 'font-lock)

;; Function for Emacs:

(if (fboundp 'x-grayscale-display-p)
    (defalias 'x-display-grayscale-p 'x-grayscale-display-p)
  (defun x-display-grayscale-p (&optional screen)
    "Return non-nil if the X screen currently in use supports grayscale."
    (and (> (x-display-planes screen) 1)
	 (memq (x-display-visual-class screen) '(static-gray gray-scale)))))

;; Functions for Lucid:

(or (fboundp 'x-display-color-p)
    (defalias 'x-display-color-p 'x-color-display-p))

(or (fboundp 'frame-parameters)
    (defalias 'frame-parameters 'screen-parameters))

(or (fboundp 'x-defined-colors)
    (defalias 'x-defined-colors 'ignore))

;; Variables:

(defvar face-lock-display-type
  (let ((display-resource (x-get-resource ".displayType" "DisplayType")))
    (cond (display-resource (intern (downcase display-resource)))
	  ((x-display-color-p) 'color)
	  ((x-display-grayscale-p) 'grayscale)
	  (t 'mono)))
  "A symbol indicating the display Emacs is running under.
The symbol should be one of `color', `grayscale' or `mono'.
If Emacs guesses this display attribute wrongly, either set this variable in
your ~/.emacs or set the resource \"Emacs.displayType\" in your ~/.Xdefaults.
See also `face-lock-background-mode' and `face-lock-face-attributes'.")

(defvar face-lock-background-mode
  (let ((bg-resource (x-get-resource ".backgroundMode" "BackgroundMode"))
	(params (frame-parameters)))
    (cond (bg-resource (intern (downcase bg-resource)))
	  ((or (string-equal (cdr (assq 'foreground-color params)) "white")
	       (string-equal (cdr (assq 'background-color params)) "black"))
	   'dark)
	  (t 'light)))
  "A symbol indicating the Emacs background brightness.
The symbol should be one of `light' or `dark'.
If Emacs guesses this frame attribute wrongly, either set this variable in
your ~/.emacs or set the resource \"Emacs.backgroundMode\" in your ~/.Xdefaults.
See also `face-lock-display-type' and `face-lock-face-attributes'.")

(defvar face-lock-face-attributes
  (let ((light-bg (eq face-lock-background-mode 'light)))
    (cond ((memq face-lock-display-type '(mono monochrome))
	   ;; Emacs 19.25's font-lock defaults:
	   ;;'((font-lock-comment-face nil nil nil t nil)
	   ;;  (font-lock-doc-string-face nil nil nil t nil)
	   ;;  (font-lock-string-face nil nil nil nil t)
	   ;;  (font-lock-keyword-face nil nil t nil nil)
	   ;;  (font-lock-function-name-face nil nil t t nil)
	   ;;  (font-lock-type-face nil nil nil t nil))
	   (list '(font-lock-comment-face nil nil t t nil)
		 '(font-lock-doc-string-face nil nil nil t nil)
		 '(font-lock-string-face nil nil nil t nil)
		 '(font-lock-keyword-face nil nil t t t)
		 (list 'font-lock-function-name-face
		       (cdr (assq 'background-color (frame-parameters)))
		       (cdr (assq 'foreground-color (frame-parameters)))
		       t nil nil)
		 '(font-lock-type-face nil nil t nil t)))
	  ((memq face-lock-display-type '(grayscale greyscale))
	   (list (list 'font-lock-comment-face
		       (if light-bg "DimGray" "Gray80") nil t t nil)
		 (list 'font-lock-doc-string-face
		       (if light-bg "Gray50" "LightGray") nil nil t nil)
		 (list 'font-lock-string-face
		       (if light-bg "Gray50" "LightGray") nil nil t nil)
		 (list 'font-lock-keyword-face
		       (if light-bg "DimGray" "Gray90") nil t t t)
		 (list 'font-lock-function-name-face
		       (cdr (assq 'background-color (frame-parameters)))
		       (cdr (assq 'foreground-color (frame-parameters)))
		       t nil nil)
		 (list 'font-lock-type-face
		       (if light-bg "DimGray" "Gray80") nil t nil t)))
	  (t				; colour
	   (list (list 'font-lock-comment-face
		       (if light-bg "Firebrick" "OrangeRed") nil nil nil nil)
		 (list 'font-lock-doc-string-face
		       (if light-bg "RosyBrown" "LightSalmon") nil nil nil nil)
		 (list 'font-lock-string-face
		       (if light-bg "RosyBrown" "LightSalmon") nil nil nil nil)
		 (list 'font-lock-keyword-face
		       (if light-bg "Purple" "LightSkyBlue") nil t nil nil)
		 (list 'font-lock-function-name-face
		       (if light-bg "Blue" "LightSteelBlue") nil t nil nil)
		 (list 'font-lock-type-face
		       (if light-bg "SeaGreen" "PaleGreen") nil t nil nil)))))
    "A list of default attributes to use for face attributes.
Each element of the list should be of the form

 (FACE FOREGROUND BACKGROUND BOLD-P ITALIC-P UNDERLINE-P)

where FACE should be one of the face symbols `font-lock-comment-face',
`font-lock-doc-string-face', `font-lock-string-face', `font-lock-keyword-face',
`font-lock-function-name-face', and `font-lock-type-face'.  A form for each of
these face symbols should be provided in the list, but other face symbols and
attributes may be given here and used in hilighting.  See `font-lock-keywords'.

Subsequent element items should be the attributes for the corresponding
face-lock faces.  Attributes FOREGROUND and BACKGROUND should be strings
\(default if nil), BOLD-P, ITALIC-P, and UNDERLINE-P should specify the
corresponding face attributes (yes if non-nil).

Emacs uses attributes based on the display type and background brightness.  See
variables `face-lock-display-type' and `face-lock-background-mode'.

Resources can be used to over-ride these face attributes.  For example, the
resource \"Emacs.font-lock-comment-face.attributeUnderline\" can be used to
specify the UNDERLINE-P attribute for face `font-lock-comment-face'.

See function `turn-on-face-lock' and interactive functions
`face-lock-mode' (an alias for `font-lock-mode'),
`face-lock-facify-buffer' (an alias for `font-lock-fontify-buffer'), and
`modify-face'.")

;; Interactive functions:

(defalias 'face-lock-mode 'font-lock-mode)
(defalias 'face-lock-facify-buffer 'font-lock-fontify-buffer)

;; Nothing to do with face-lock anymore, but we might as well provide it.
(defun modify-face (face foreground background bold-p italic-p underline-p)
  "Change the display attributes for face FACE.
FOREGROUND and BACKGROUND should be color strings.  (Default color if nil.)
BOLD-P, ITALIC-P, and UNDERLINE-P specify whether the face should be set bold,
in italic, and underlined, respectively.  (Yes if non-nil.)
If called interactively, prompts for a face and face attributes."
  (interactive
   (let* ((completion-ignore-case t)
	  (face		(symbol-name (read-face-name "Face: ")))
	  (foreground	(completing-read
			 (concat "Face " face " set foreground: ")
			 (mapcar 'list (x-defined-colors))))
	  (background	(completing-read
			 (concat "Face " face " set background: ")
			 (mapcar 'list (x-defined-colors))))
	  (bold-p	(y-or-n-p (concat "Face " face ": set bold ")))
	  (italic-p	(y-or-n-p (concat "Face " face ": set italic ")))
	  (underline-p	(y-or-n-p (concat "Face " face ": set underline "))))
     (if (string-equal background "") (setq background nil))
     (if (string-equal foreground "") (setq foreground nil))
     (message "Face %s: %s" face
      (mapconcat 'identity
       (delq nil
	(list (and foreground (concat (downcase foreground) " foreground"))
	      (and background (concat (downcase background) " background"))
	      (and bold-p "bold") (and italic-p "italic")
	      (and underline-p "underline"))) ", "))
     (list (intern face) foreground background bold-p italic-p underline-p)))
  (condition-case nil (set-face-foreground face foreground) (error nil))
  (condition-case nil (set-face-background face background) (error nil))
  (set-face-bold-p face bold-p)
  (set-face-italic-p face italic-p)
  (set-face-underline-p face underline-p)
  (and (interactive-p) (redraw-display)))

;; Functions:

(if (fboundp 'turn-on-font-lock)
    (defalias 'turn-on-face-lock 'turn-on-font-lock)
  (defun turn-on-face-lock ()
    "Unconditionally turn on Face Lock mode."
    (face-lock-mode t)))

(defun set-face-bold-p (face bold-p &optional frame)
  "Specify whether face FACE is bold.  (Yes if BOLD-P is non-nil.)"
  (if (save-match-data (string-match "Lucid" (emacs-version)))
      (funcall (if bold-p 'make-face-bold 'make-face-unbold) face)
    (funcall (if bold-p 'make-face-bold 'make-face-unbold) face frame t)))

(defun set-face-italic-p (face italic-p &optional frame)
  "Specify whether face FACE is italic.  (Yes if ITALIC-P is non-nil.)"
  (if (save-match-data (string-match "Lucid" (emacs-version)))
      (funcall (if italic-p 'make-face-italic 'make-face-unitalic) face)
    (funcall (if italic-p 'make-face-italic 'make-face-unitalic) face frame t)))

(defun face-lock-make-face (face-attributes)
  "Make a face from FACE-ATTRIBUTES.
FACE-ATTRIBUTES should be like an element `face-lock-face-attributes', so that
the face name is the first item in the list.  A variable with the same name as
the face is also created; its value is the face name."
  (let* ((face (nth 0 face-attributes))
	 (face-name (symbol-name face))
	 (lucid-p (save-match-data (string-match "Lucid" (emacs-version))))
	 (set-p (if lucid-p
		  (function (lambda (face-name resource)
		   (x-get-resource (concat face-name ".attribute" resource)
				   (concat "Face.Attribute" resource)
				   'string)))
		(function (lambda (face-name resource)
		 (x-get-resource (concat face-name ".attribute" resource)
				 (concat "Face.Attribute" resource))))))
	 (on-p (function (lambda (face-name resource)
		(let ((set (funcall set-p face-name resource)))
		  (and set (member (downcase set) '("on" "true"))))))))
    (make-face face)
    ;; Set attributes not set from X resources (and therefore `make-face').
    (or (funcall set-p face-name "Foreground")
	(condition-case nil
	    (set-face-foreground face (nth 1 face-attributes))
	  (error nil)))
    (or (funcall set-p face-name "Background")
	(condition-case nil
	    (set-face-background face (nth 2 face-attributes))
	  (error nil)))
    (set-face-bold-p face (if (funcall set-p face-name "Bold")
			      (funcall on-p face-name "Bold")
			    (nth 3 face-attributes)))
    (set-face-italic-p face (if (funcall set-p face-name "Italic")
				(funcall on-p face-name "Italic")
			      (nth 4 face-attributes)))
    (or (funcall set-p face-name "Underline")
	(set-face-underline-p face (nth 5 face-attributes)))
    (or lucid-p (set face face))))

;; Install ourselves:

(mapcar 'face-lock-make-face face-lock-face-attributes)

(setcdr (assq 'font-lock-mode minor-mode-alist) '(" Face"))

;; Provide ourselves:

(provide 'face-lock)

;;; face-lock.el ends here


