;;; lazy-lock.el --- Lazy demand-driven fontification for fast Font Lock mode.

;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;; Author: Simon Marshall <simon@gnu.ai.mit.edu>
;; Keywords: faces files
;; Version: 1.14

;; LCD Archive Entry:
;; lazy-lock|Simon Marshall|simon@gnu.ai.mit.edu|
;; Lazy Font Lock mode (with fast demand-driven fontification).|
;; 13-Oct-95|1.14|~/modes/lazy-lock.el.Z|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;;
;; To make visiting buffers in `font-lock-mode' faster by making fontification
;; be demand-driven and stealthy.
;; Fontification only occurs when, and where, necessary.
;;
;; See caveats and feedback below.  See also the defer-lock and fast-lock
;; packages.  (But don't use lazy-lock.el and fast-lock.el at the same time!)

;; Installation:
;; 
;; Put this file somewhere where Emacs can find it (i.e., in one of the paths
;; in your `load-path'), `byte-compile-file' it, and put in your ~/.emacs:
;;
;; (autoload 'turn-on-lazy-lock "lazy-lock"
;;   "Unconditionally turn on Lazy Lock mode.")
;;
;; (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
;;
;; Start up a new Emacs and use font-lock as usual (except that you can use the
;; so-called "gaudier" fontification regexps on big files without frustration).
;;
;; In a buffer (which has `font-lock-mode' enabled) which is at least
;; `lazy-lock-minimum-size' characters long, only the visible portion of the
;; buffer will be fontified.  Motion around the buffer will fontify those
;; visible portions that were not previous fontified.  If the variable
;; `lazy-lock-hide-invisible' is non-nil, redisplay will be delayed until after
;; fontification.  Otherwise, text that has not yet been fontified is displayed
;; in `lazy-lock-invisible-foreground'.
;;
;; If stealth fontification is enabled, fontification will occur in invisible
;; parts of the buffer after `lazy-lock-stealth-time' seconds of idle time.

;; Advanced Use:
;;
;; You can also do fancy things with `advice'.  For example, to fontify when
;; dragging the scroll-bar in Emacs, you could put in your ~/.emacs:
;;
;; (autoload 'lazy-lock-post-command-fontify-windows "lazy-lock")
;;
;; (defadvice scroll-bar-drag-1 (after fontify-window activate compile)
;;   (let ((lazy-lock-walk-windows nil) (lazy-lock-hide-invisible nil))
;;     (lazy-lock-post-command-fontify-windows)))
;;
;; Or to fontify when the Debugger pops up a source code window:
;;
;; (autoload 'lazy-lock-fontify-walk-windows "lazy-lock")
;;
;; (defadvice gud-display-line (after fontify-window activate compile)
;;   (let ((lazy-lock-walk-windows t) (lazy-lock-hide-invisible t))
;;     (lazy-lock-fontify-walk-windows)))
;;
;; Scott Byer <byer@mv.us.adobe.com> suggested this to fontify the visible part
;; of an insertion only:
;;
;; (defvar lazy-lock-insert-commands
;;   '(yank yank-pop clipboard-yank hilit-yank hilit-yank-pop 
;;     mail-yank-original mouse-yank-at-click mouse-yank-secondary
;;     yank-rectangle)
;;   "A list of insertion commands.")
;;
;; (defadvice font-lock-after-change-function (around fontify-insertion
;;                                             activate compile)
;;   (if (or (not (memq this-command lazy-lock-insert-commands))
;; 	  (and (pos-visible-in-window-p beg) (pos-visible-in-window-p end)))
;;       ad-do-it
;;     (let ((this-command 'ignore))
;;       (put-text-property beg end 'fontified nil)
;;       (lazy-lock-fontify-window))))
;;
;; Let me know if you use any other `advice' and I'll put it here.  Thanks.
;;
;; These kinds of things with `advice' aren't done automatically because they
;; cause large packages (advice.el plus bytecomp.el and friends) to be loaded.

;; Caveats:
;;
;; Lazy Lock mode does not work efficiently with Outline mode.  This is because
;; when in Outline mode, although text may be hidden (not visible in the
;; window), the text is visible to Emacs Lisp code (not surprisingly) and Lazy
;; Lock fontifies it mercilessly.  Hopefully this will be fixed one day.
;;
;; Lazy Lock mode does not fontify windows as they appear:
;;
;; 1.  With `query-replace' or `ispell-*', as Lazy Lock only knows about point
;; motion after the command exits.
;;
;; 2.  When displayed by gud.el (the Grand Unified Debugger), as they are
;; displayed via a process sentinel.  See `Advanced Use' above.
;;
;; 3.  In XEmacs 19.12, when the last command was invoked via a mouse event,
;; because of a bug/feature in/of `sit-for'.
;;
;; 4.  In other random situations that I don't know about (yet).
;;
;; If you have `lazy-lock-hide-invisible' you may notice that redisplay occurs
;; before fontification regardlessly.  This is due to other packages sitting on
;; `post-command-hook' and provoking redisplay.  If you use these packages, you
;; can't use `lazy-lock-hide-invisible'.
;;
;; If you have `lazy-lock-hide-invisible' and use scrollbar scrolling using
;; Emacs 19, hidden text will not be fontified as it becomes visible.  It is
;; expected that Emacs 19 will provide the necessary hooks in future, to solve
;; this problem and the problem above.
;;
;; Unless otherwise stated, "Emacs 19.X" means versions up to and including X.
;;
;; In Emacs 19.25, one `window-start'/`window-end' bug means that if you open a
;; file in another frame (such as via `find-tag-other-frame'), the whole buffer
;; is fontified regardless.  Upgrade!
;;
;; In Emacs 19.25, fontification by stealth is turned off because of a fatal
;; bug in `previous-single-property-change'.  Upgrade!
;;
;; In Emacs 19.28, if you see a message in the minibuffer of the form
;;  "Fontifying window... done.  (Restarted in foo.c)"
;; it means the Garbage Collector has marked some (subsequently used) text
;; properties.  Lazy Lock attempts to recover the situation by restarting in
;; that buffer.  Unfortunately, that buffer will be left in a writable and
;; modified state.  Also, other windows may not be fontified when this happens.
;; To reduce the frequency of this bug occuring, increase in your ~/.emacs the
;; value of `gc-cons-threshold' to, say, 1Meg, e.g.:
;;
;; (setq gc-cons-threshold (* 1024 1024))
;;
;; The solution is to upgrade!  (With thanks to Kevin Broadey for help here.)
;;
;; For XEmacs 19.11 and Lucid Emacs 19.10 users, lazy-lock sort-of works.
;; There are bugs in text property and point/window primatives.  Upgrade!

;; Feedback:
;;
;; Feedback is welcome.
;; To submit a bug report (or make comments) please use the mechanism provided:
;;
;; M-x lazy-lock-submit-bug-report RET

;; History:
;;
;; 0.01--1.00:
;; - Changed name from fore-lock to lazy-lock.  Shame though.
;; - Dropped `advice'-wrapping completely.  Ask me if you're interested in it.
;; - Made `lazy-lock-mode' ignore `post-command-hook' and `buffer-file-name'.
;; - Made `lazy-lock-fontify-window' check `lazy-lock-mode' and `this-command'.
;; - Made `lazy-lock-fontify-window' redisplay via `sit-for'.
;; - Added `lazy-lock-minimum-size' to control `lazy-lock-mode'.
;; 1.00--1.01:
;; - Added `lazy-lock-fontify-buffer'.
;; - Made `lazy-lock-fontify-window' ignore `lazy-lock-mode'.
;; - Made `lazy-lock-fontify-window' suspicious of `window-' favourites again.
;; - Added `lazy-lock-delay-commands' (idea from William G. Dubuque).
;; - Added `lazy-lock-ignore-commands' for completeness.
;; - Added `lazy-lock-continuity-time' for normal input delay.
;; 1.01--1.02:
;; - Made `lazy-lock-fontify-window' cope with multiple unfontified regions.
;; - Made `lazy-lock-mode' remove `fontified' properties if turned off.
;; - Made `lazy-lock-fontify-window' fontify by lines.
;; - Added `lazy-lock-cache-position' buffer local to detect visibility change.
;; - Added `lazy-lock-post-command-hook' to do the waiting.
;; - Made `lazy-lock-fontify-window' just do the fontification.
;; - Made `lazy-lock-mode' append `lazy-lock-post-command-hook'.
;; - Added `lazy-lock-walk-windows' to hack multi-window motion.
;; - Made `lazy-lock-post-command-hook' `walk-windows' if variable is non-nil.
;; - Removed `lazy-lock-ignore-commands' since insertion may change window.
;; - Added `lazy-lock-fontify-stealthily' and `lazy-lock-stealth-time'.
;; - Made `lazy-lock-post-command-hook' use them.
;; 1.02--1.03:
;; - Made `lazy-lock-fontify-stealthily' do `forward-line' not `previous-line'.
;; - Made `lazy-lock-fontify-stealthily' `move-to-window-line' first.
;; - Made `lazy-lock-fontify-stealthily' use `text-property-any' for region.
;; - Made `lazy-lock-post-command-hook' loop on `lazy-lock-fontify-stealthily'.
;; 1.03--1.04:
;; - Made `lazy-lock-mode' reset `lazy-lock-cache-position'.
;; - Made `lazy-lock-post-command-hook' `widen' for `if' `text-property-any'.
;; - Made `lazy-lock-fontify-stealthily' return `text-property-any'.
;; - Added `lazy-lock-percent-fontified' for a/be-musement.
;; - Made `lazy-lock-post-command-hook' use it.
;; - Made `lazy-lock-mode' use `make-local-hook' etc. if available.
;; - Made `lazy-lock-mode' use `before-revert-hook' and `after-revert-hook'.
;; - Made `lazy-lock-post-command-hook' protect `deactivate-mark'.
;; - Adds `lazy-lock-post-command-hook' globally to `post-command-hook'.
;; 1.04--1.05:
;; - Made `lazy-lock-mode' test `make-local-hook' not `emacs-minor-version'.
;; 1.05--1.06:
;; - Added `lazy-lock-ignore-commands' for commands that leave no event but do.
;; - Made `lazy-lock-post-command-hook' check `lazy-lock-ignore-commands'.
;; 1.06--1.07:
;; - Removed `before-revert-hook' and `after-revert-hook' use.
;; 1.07--1.08:
;; - Added `lazy-lock-submit-bug-report'.
;; - Made `lazy-lock-post-command-hook' check `executing-macro'.
;; - Made it sort-of/almost work for XEmacs (help from Jonas Jarnestrom).
;; - XEmacs: Fix `text-property-not-all' (fix based on fast-lock.el 3.05 fix).
;; - XEmacs: Set `font-lock-no-comments' and alias `frame-parameters'.
;; - Made `byte-compile-warnings' omit `unresolved' on compilation.
;; - Made `lazy-lock-post-command-hook' protect `buffer-undo-list'.
;; - Moved `deactivate-mark' and `buffer-undo-list' protection to functions.
;; - Added `lazy-lock-invisible-foreground' (idea from Boris Goldowsky).
;; - XEmacs: Fix to use `text-property-not-all' t, not `text-property-any' nil.
;; - Made `lazy-lock-percent-fontified' return `round' to an integer.
;; - XEmacs: Fix `text-property-any' (fix and work around for a bug elsewhere).
;; - XEmacs: Fix `lazy-lock-submit-bug-report' for reporter.el & vm-window.el.
;; - XEmacs: Made `lazy-lock-fontify-window' loop `while' `<' not `/='.
;; - Use `font-lock-after-change-function' to do the fontification.
;; 1.08--1.09:
;; - Made `lazy-lock-post-command-hook' protect with `condition-case'.
;; - Made `lazy-lock-cache-start' to cache `window-start'.
;; - Made `lazy-lock-fontify-window' check and cache `lazy-lock-cache-start'.
;; - Renamed `lazy-lock-cache-position' to `lazy-lock-cache-end'.
;; - XEmacs: Fix for `font-lock-after-change-function'.
;; - Adds `lazy-lock-post-command-hook' globally to `window-setup-hook'.
;; 1.09--1.10:
;; - Made `buffer-file-name' be `let' to prevent supersession (Kevin Broadey).
;; - Made `lazy-lock-submit-bug-report' `require' reporter (Ilya Zakharevich).
;; - Made `lazy-lock-mode' and `turn-on-lazy-lock' succeed `autoload' cookies.
;; - Added `lazy-lock-fontify-walk-windows' for walking window fontification.
;; - Added `lazy-lock-fontify-walk-stealthily' for walking stealth.
;; - Removed `move-to-window-line' from `lazy-lock-fontify-stealthily'.
;; - Made `lazy-lock-percent-fontified' use `truncate' rather than `round'.
;; - Added other `*-argument' to `lazy-lock-ignore-commands' (Kevin Broadey).
;; - Made `lazy-lock-fontify-stealthily' not assume buffer is part `fontified'.
;; - Emacs: Fix for `font-lock-fontify-region'.
;; - Made `lazy-lock-post-command-hook' check for minibuffer (Kevin Broadey).
;; - Added `lazy-lock-stealth-nice' for niceness during stealth fontification.
;; - Added `lazy-lock-stealth-lines' for chunks of stealth fontification.
;; 1.10--1.11: incorporated hack by Ben Wing from William Dubuque's fontifly.el
;; - Made `lazy-lock-fontify-stealthily' see a non `fontified' preceding line.
;; - XEmacs: Fix `text-property-any' and `text-property-not-all' (Ben Wing).
;; - XEmacs: Fix `lazy-lock-continuity-time' (Ben Wing).
;; - Added awful `lazy-lock-running-xemacs-p' (Ben Wing).
;; - Made loading set `emacs-minor-version' if it's not bound.
;; - Added `lazy-lock-hide-invisible' to control redisplay.
;; - Made `lazy-lock-post-command-hook' use it in `sit-for' (Ben Wing).
;; - Made `lazy-lock-fontify-window' move relative to `end-of-line' if non-nil.
;; - Added `lazy-lock-fontify-region' so packages can ensure fontification.
;; - Made `lazy-lock-fontify-walk-stealthily' do stealth widening.
;; - Made `lazy-lock-fontify-stealthily' always do adjacent preceding regions.
;; - Added `lazy-lock-after-fontify-buffer'.
;; - XEmacs: Removed `font-lock-no-comments' incompatibility code.
;; - Removed `lazy-lock-delay-time' and `lazy-lock-delay-commands'.
;; - Removed `lazy-lock-post-command' and split the functionality.
;; - Adds `lazy-lock-post-command-fontify-windows' on first.
;; - Adds `lazy-lock-post-command-fontify-stealthily' on last.
;; - Made `lazy-lock-mode' ensure both first and last on `post-command-hook'.
;; - Made `lazy-lock-mode' ensure `font-lock-mode' is on.
;; - Wrap `lazy-lock-post-command-fontify-stealthily' for errors (David Karr).
;; - Added `calcDigit-key' to `lazy-lock-ignore-commands' (Bob Glickstein).
;; - Wrap `lazy-lock-running-xemacs-p' with `eval-and-compile' (Erik Naggum).
;; - XEmacs: Fix use of `previous-single-property-change' (Jim Thompson).
;; - XEmacs: Fix `next-single-property-change' fix for 19.11 (Jim Thompson).
;; - Added `lazy-lock-post-resize-fontify-windows' to fontify on resizing.
;; - Adds globally to `window-size-change-functions'.
;; - Added `lazy-lock-post-setup-fontify-windows' to fontify after start up.
;; - Adds globally to `window-setup-hook'.
;; - Made `lazy-lock-post-command-fontify-windows' check for `input-pending-p'.
;; - Made `save-selected-window' to restore the `selected-window'.
;; - Use `save-selected-window' rather than `save-window-excursion'.
;; 1.11--1.12:
;; - Made `lazy-lock-post-command-fontify-windows' do `set-buffer' first.
;; - Made `lazy-lock-fontify-stealthily' respect narrowing before point.
;; - Added `lazy-lock-post-setup-ediff-control-frame' for Ediff control frame.
;; - Adds globally to `ediff-after-setup-control-frame-hooks'.
;; - Wrap `save-selected-window' with `save-excursion' for `current-buffer'.
;; 1.12--1.13:
;; - XEmacs: Add `lazy-lock-after-fontify-buffer' to the Font Lock hook.
;; - Made `buffer-file-truename' also wrapped for supersession (Rick Sladkey).
;; - Made `font-lock-beginning-of-syntax-function' wrapped for fontification.
;; - Added `lazy-lock-stealth-verbose' (after harassment from Ben Wing).
;; - XEmacs: Made `font-lock-verbose' wrapped for stealth fontification.
;; 1.13--1.14:
;; - Wrap `lazy-lock-colour-invisible' for `set-face-foreground' (Jari Aalto).

(require 'font-lock)

(eval-when-compile
  ;; Only `require' so `ediff-multiframe-setup-p' is expanded at compile time.
  (condition-case nil (require 'ediff) (file-error))
  ;; Well, shouldn't Lazy Lock be as lazy as possible?
  ;(setq byte-compile-dynamic t byte-compile-dynamic-docstrings t)
  ;; Shut Emacs' byte-compiler up (cf. stop me getting mail from users).
  (setq byte-compile-warnings '(free-vars callargs redefine)))

(defun lazy-lock-submit-bug-report ()
  "Submit via mail a bug report on lazy-lock.el."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report "simon@gnu.ai.mit.edu" "lazy-lock 1.14"
     '(lazy-lock-walk-windows lazy-lock-continuity-time
       lazy-lock-stealth-time lazy-lock-stealth-nice
       lazy-lock-stealth-lines lazy-lock-stealth-verbose
       lazy-lock-hide-invisible lazy-lock-invisible-foreground
       lazy-lock-minimum-size lazy-lock-ignore-commands)
     nil nil
     (concat "Hi Si.,

I want to report a bug.  I've read the `Bugs' section of `Info' on Emacs, so I
know how to make a clear and unambiguous report.  To reproduce the bug:

Start a fresh Emacs via `" invocation-name " -no-init-file -no-site-file'.
In the `*scratch*' buffer, evaluate:"))))

;; Let's define `emacs-minor-version' if no-one else has.
(if (not (boundp 'emacs-minor-version))
    (eval-and-compile
      (defconst emacs-minor-version
	(save-match-data
	  (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
	  (string-to-int
	   (substring emacs-version (match-beginning 1) (match-end 1)))))))

;; Yuck, but we make so much use of this variable it's probably worth it.
(eval-and-compile
  (defconst lazy-lock-running-xemacs-p
    (not (null (save-match-data (string-match "Lucid" emacs-version))))))

(defvar lazy-lock-cache-start nil)	; for window fontifiction
(defvar lazy-lock-cache-end nil)	; for window fontifiction
(defvar lazy-lock-cache-continue nil)	; for stealth fontifiction
(defvar lazy-lock-mode nil)		; for modeline

;; User Variables:

(defvar lazy-lock-minimum-size (* 25 1024)
  "*If non-nil, the minimum size for buffers.
Only buffers more than this can have demand-driven fontification.
If nil, means size is irrelevant.")

(defvar lazy-lock-walk-windows t
  "*If non-nil, fontify windows other than the selected window.
If `all-frames', fontify windows even on other frames.
A non-nil value slows down redisplay.")

;; XEmacs 19.11 and below exercise a bug in the Xt event loop.
(defvar lazy-lock-continuity-time
  (if (or (not lazy-lock-running-xemacs-p) (> emacs-minor-version 11))
      0
    (if (featurep 'lisp-float-type) 0.001 1))
  "*Time in seconds to delay before normal window fontification.
Window fontification occurs if there is no input within this time.")

;; `previous-single-property-change' at `point-min' up to Emacs 19.25 is fatal.
;; `text-property-any', `text-property-not-all' and
;; `next-single-property-change' up to XEmacs 19.11 are too broke.
(defvar lazy-lock-stealth-time
  (if (> emacs-minor-version (if lazy-lock-running-xemacs-p 11 25)) 30)
  "*Time in seconds to delay before beginning stealth fontification.
Stealth fontification occurs if there is no input within this time.
If nil, means no fontification by stealth.")

(defvar lazy-lock-stealth-lines
  (cond ((boundp 'font-lock-maximum-decoration)
	 (if font-lock-maximum-decoration 75 150))
	((boundp 'font-lock-use-maximal-decoration)
	 (if font-lock-use-maximal-decoration 50 100))
	(t
	 50))
  "*If non-nil, the maximum size of a chunk of stealth fontification.
Each iteration of stealth fontification can fontify this number of lines.
To speed up input response during stealth fontification, at the cost of stealth
taking longer to fontify, you could reduce the value of this variable.
If nil, means use `window-height' for the maximum chunk size.")

(defvar lazy-lock-stealth-nice (if (featurep 'lisp-float-type) 0.125 1)
  "*Time in seconds to pause during chunks of stealth fontification.
To reduce machine load during stealth fontification, at the cost of stealth
taking longer to fontify, you could increase the value of this variable.")

(defvar lazy-lock-stealth-verbose font-lock-verbose
  "*If non-nil, means stealth fontification should show status messages.")

(defvar lazy-lock-ignore-commands
  (append
   ;; Standard commands...
   '(universal-argument digit-argument negative-argument
     isearch-other-control-char isearch-other-meta-char)
   ;; And some resulting from non-standard packages...
   (if (fboundp 'calc) '(calcDigit-key)))
  "A list of commands after which fontification should not occur.
To speed up typing response, at the cost of Lazy Lock not fontifying when
insertion causes scrolling, you could add `self-insert-command' to this list.")

(defvar lazy-lock-hide-invisible lazy-lock-running-xemacs-p
  "*If non-nil, hide invisible text while it is fontified.
If non-nil, redisplay is delayed until after fontification occurs.  If nil,
text is shown (in `lazy-lock-invisible-foreground') while it is fontified.
A non-nil value slows down redisplay and can slow down cursor motion.")

(defvar lazy-lock-invisible-foreground "gray50" 
  "The foreground colour to use to display invisible text.
If nil, the default foreground is used.  If t, the default background is used.
If a string, it should be a colour to use (either its name or its RGB value).
Invisible text is momentarily seen (if `lazy-lock-hide-invisible' is nil) when
scrolling into unfontified regions.")

;; User Functions:

;;;###autoload
(defun lazy-lock-mode (&optional arg)
  "Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive and the buffer
is at least `lazy-lock-minimum-size' characters long.

When Lazy Lock mode is enabled, fontification is demand-driven and stealthy:

 - Fontification occurs in visible parts of buffers when necessary.
   Occurs if there is no input after pausing for `lazy-lock-continuity-time'.

 - Fontification occurs in invisible parts when Emacs has been idle.
   Occurs if there is no input after pausing for `lazy-lock-stealth-time'.

If `lazy-lock-hide-invisible' is non-nil, text is not displayed until it is
fontified, otherwise it is displayed in `lazy-lock-invisible-foreground'.

See also variables `lazy-lock-walk-windows' and `lazy-lock-ignore-commands' for
window (scroll) fontification, and `lazy-lock-stealth-lines',
`lazy-lock-stealth-nice' and `lazy-lock-stealth-verbose' for stealth
fontification.

Use \\[lazy-lock-submit-bug-report] to send bug reports or feedback."
  (interactive "P")
  (set (make-local-variable 'lazy-lock-mode)
       (and (<= (or lazy-lock-minimum-size 0) (buffer-size))
	    (if arg (> (prefix-numeric-value arg) 0) (not lazy-lock-mode))))
  (if (and lazy-lock-mode (not font-lock-mode))
      ;; Turned on `lazy-lock-mode' rather than using `font-lock-mode-hook'.
      (progn
	(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
	(font-lock-mode 1))
    ;; Make sure that we're in the correct positions to avoid hassle.
    (remove-hook 'post-command-hook 'lazy-lock-post-command-fontify-windows)
    (remove-hook 'post-command-hook 'lazy-lock-post-command-fontify-stealthily)
    (add-hook 'post-command-hook 'lazy-lock-post-command-fontify-windows)
    (add-hook 'post-command-hook 'lazy-lock-post-command-fontify-stealthily t)
    ;; Let's get down to business.
    (if (not lazy-lock-mode)
	(let ((modified (buffer-modified-p)) (inhibit-read-only t)
	      (buffer-undo-list t)
	      deactivate-mark buffer-file-name buffer-file-truename)
	  (remove-text-properties (point-min) (point-max) '(fontified nil))
	  (or modified (set-buffer-modified-p nil)))
      (if (and (not lazy-lock-hide-invisible) lazy-lock-invisible-foreground)
	  (lazy-lock-colour-invisible))
      (set (make-local-variable 'lazy-lock-cache-start) 0)
      (set (make-local-variable 'lazy-lock-cache-end) 0)
      (set (make-local-variable 'font-lock-fontified) t))))

;;;###autoload
(defun turn-on-lazy-lock ()
  "Unconditionally turn on Lazy Lock mode."
  (lazy-lock-mode 1))

(if (< emacs-minor-version (if lazy-lock-running-xemacs-p 12 29))
    ;; We don't need this in Emacs 19.29 or XEmacs 19.12.
    (defun lazy-lock-fontify-buffer ()
      "Fontify the current buffer where necessary."
      (interactive)
      (lazy-lock-fontify-region (point-min) (point-max))))

;; API Functions:

(defun lazy-lock-fontify-region (start end &optional buffer)
  "Fontify between START and END in BUFFER where necessary."
  (save-excursion
    (and buffer (set-buffer buffer))
    (save-restriction
      (narrow-to-region start end)
      (let ((lazy-lock-stealth-lines (count-lines start end)))
	(while (text-property-not-all start end 'fontified t)
	  (lazy-lock-fontify-stealthily))))))

(defun lazy-lock-after-fontify-buffer ()
  ;; Mark the buffer as `fontified'.
  (let ((modified (buffer-modified-p)) (inhibit-read-only t)
	(buffer-undo-list t)
	deactivate-mark buffer-file-name buffer-file-truename)
    (put-text-property (point-min) (point-max) 'fontified t)
    (or modified (set-buffer-modified-p nil))))

;; Just a cleaner-looking way of coping with Emacs' and XEmacs' `sit-for'.
(defmacro lazy-lock-sit-for (seconds &optional nodisp)
  (if lazy-lock-running-xemacs-p
      (` (sit-for (, seconds) (, nodisp)))
    (` (sit-for (, seconds) 0 (, nodisp)))))

;; Using `save-window-excursion' provokes `window-size-change-functions'.
;; I prefer `save-walking-excursion', of course, because I have a warped mind.
(if (fboundp 'save-selected-window)
    nil
  (eval-and-compile
    (defmacro save-selected-window (&rest body)
      "Execute the BODY forms, restoring the selected window.
Does not restore the value of point in the selected window, or anything else."
      (` (let ((original-window (selected-window)))
	   (unwind-protect
	       (progn (,@ body))
	     (select-window original-window))))))
  (put 'save-selected-window 'lisp-indent-function 0))

;; Functions for hooks:

(defun lazy-lock-post-command-fontify-windows ()
  ;; We might not be where we think we are, since `post-command-hook' is run
  ;; before `command_loop_1' makes sure we have the correct buffer selected.
  (set-buffer (window-buffer))
  ;; Do groovy things if (a) not in a macro, (b) no input pending, (c) got a
  ;; real command, (d) not in the minibuffer, and (e) no input after waiting
  ;; for `lazy-lock-continuity-time'.
  (if (or executing-kbd-macro
	  (input-pending-p)
	  (memq this-command lazy-lock-ignore-commands)
	  (window-minibuffer-p (selected-window)))
      (setq lazy-lock-cache-continue nil)
    (setq lazy-lock-cache-continue t)
    (if (lazy-lock-sit-for lazy-lock-continuity-time lazy-lock-hide-invisible)
	;; Do the visible parts of the buffer(s), i.e., the window(s).
	(if (or (not lazy-lock-walk-windows)
		(and (eq lazy-lock-walk-windows t) (one-window-p t)))
	    (if lazy-lock-mode (condition-case nil (lazy-lock-fontify-window)))
	  (lazy-lock-fontify-walk-windows)))))

(defun lazy-lock-post-command-fontify-stealthily ()
  ;; Do groovy things if (a-d) above, (e) not moving the mouse, and (f) no
  ;; input after after waiting for `lazy-lock-stealth-time'.
  (if (and lazy-lock-cache-continue lazy-lock-stealth-time)
      (condition-case data
	  (if (lazy-lock-sit-for lazy-lock-stealth-time)
	      ;; Do the invisible parts of buffers.
	      (lazy-lock-fontify-walk-stealthily)) 
	(error (message "Fontifying stealthily... %s" data)))))

(defun lazy-lock-post-resize-fontify-windows (frame)
  ;; Fontify all windows in FRAME.
  (let ((lazy-lock-walk-windows t) executing-kbd-macro this-command)
    (save-excursion
      (save-selected-window
	(select-frame frame)
	(lazy-lock-post-command-fontify-windows)))))

(defun lazy-lock-post-setup-emacs-fontify-windows ()
  ;; Fontify all windows in all frames.
  (let ((lazy-lock-walk-windows 'all-frames) executing-kbd-macro this-command)
    (lazy-lock-post-command-fontify-windows)))

(defun lazy-lock-post-setup-ediff-control-frame ()
  ;; Fontify all windows in all frames when using the Ediff control frame.
  (make-local-variable 'lazy-lock-walk-windows)
  (setq lazy-lock-walk-windows (if (ediff-multiframe-setup-p) 'all-frames t))
  ;; Make sure that we're in the correct positions to avoid hassle.
  (remove-hook 'post-command-hook 'lazy-lock-post-command-fontify-windows)
  (remove-hook 'post-command-hook 'lazy-lock-post-command-fontify-stealthily)
  (add-hook 'post-command-hook 'lazy-lock-post-command-fontify-windows)
  (add-hook 'post-command-hook 'lazy-lock-post-command-fontify-stealthily t))

;; Functions for fontification:

(defun lazy-lock-fontify-window ()
  ;; Fontify the visible part of the buffer where necessary.
  (let ((ws (if lazy-lock-hide-invisible
		(save-excursion
		  (end-of-line) (forward-line (- (window-height))) (point))
	      (min (max (window-start) (point-min)) (point-max))))
	(we (if lazy-lock-hide-invisible
		(save-excursion
		  (end-of-line) (forward-line (window-height)) (point))
	      (min (max (1- (window-end)) (point-min)) (point-max)))))
    (if (or (/= ws lazy-lock-cache-start) (/= we lazy-lock-cache-end))
	;; Find where we haven't `fontified' before.
	(let* ((start (or (text-property-not-all ws we 'fontified t) ws))
	       (end (or (text-property-any start we 'fontified t) we))
	       (modified (buffer-modified-p)) (inhibit-read-only t)
	       ;; We do the following to prevent: undo list addition; region
	       ;; highlight disappearance; supersession/locking checks.
	       (buffer-undo-list t)
	       deactivate-mark buffer-file-name buffer-file-truename
	       ;; Ensure Emacs 19.30 syntactic fontification is always correct.
	       font-lock-beginning-of-syntax-function
	       ;; Prevent XEmacs 19.13 during fontification from messages.
	       font-lock-verbose)
	  (while (< start end)
	    ;; Fontify and flag the region as `fontified'.
	    (font-lock-after-change-function start end 0)
	    (put-text-property start end 'fontified t)
	    ;; Find the next region.
	    (setq start (or (text-property-not-all ws we 'fontified t) ws)
		  end (or (text-property-any start we 'fontified t) we)))
	  (setq lazy-lock-cache-start ws lazy-lock-cache-end we)
	  (or modified (set-buffer-modified-p nil))))))

(defun lazy-lock-fontify-walk-windows ()
  ;; Fontify windows in all required by walking through them.
  (save-excursion
    (save-selected-window
      (condition-case nil
	  (walk-windows
	   (function (lambda (window)
		       (select-window window)
		       (if lazy-lock-mode (lazy-lock-fontify-window))))
	   'no-minibuf (eq lazy-lock-walk-windows 'all-frames))
	(wrong-type-argument
	 ;; Looks like the Emacs 19.28 Garbage Collection bug has hit town.
	 ;; Completely remove all text properties and restart.
	 (set-text-properties (point-min) (point-max) nil)
	 (turn-on-lazy-lock)
	 (lazy-lock-fontify-window)
	 (message "Fontifying window... done.  (Restarted in %s)"
		  (buffer-name)))))))

(defun lazy-lock-fontify-stealthily ()
  ;; Fontify an invisible part of the buffer where necessary.
  (save-excursion
    ;; Move to the end in case the character to the left is not `fontified'.
    (end-of-line)
    ;; Find where the next and previous regions not `fontified' begin and end.
    (let ((next (text-property-not-all (point) (point-max) 'fontified t))
	  (prev (let ((p (previous-single-property-change (point) 'fontified)))
		  (and p (> p (point-min)) p)))
	  (modified (buffer-modified-p)) (inhibit-read-only t) start end
	  ;; We do the following to prevent: undo list addition; region
	  ;; highlight disappearance; supersession/locking checks.
	  (buffer-undo-list t)
	  deactivate-mark buffer-file-name buffer-file-truename
	  ;; Ensure Emacs 19.30 syntactic fontification is always correct.
	  font-lock-beginning-of-syntax-function
	  ;; Prevent XEmacs 19.13 during fontification from spewing messages.
	  font-lock-verbose)
      (cond ((and (null next) (null prev))
	     ;; Nothing has been `fontified' yet.
	     (beginning-of-line 1) (setq start (point))
	     (forward-line (or lazy-lock-stealth-lines (window-height)))
	     (setq end (point)))
	    ((or (null prev)
		 (and next (> (- (point) prev) (- next (point)))))
	     ;; The next region is the nearest not `fontified'.
	     (goto-char next) (beginning-of-line 1) (setq start (point))
	     (forward-line (or lazy-lock-stealth-lines (window-height)))
	     ;; Maybe the region is already partially `fontified'.
	     (setq end (or (text-property-any next (point) 'fontified t)
			   (point))))
	    (t
	     ;; The previous region is the nearest not `fontified'.
	     (goto-char prev) (forward-line 1) (setq end (point))
	     (forward-line (- (or lazy-lock-stealth-lines (window-height))))
	     ;; Maybe the region is already partially `fontified'.
	     (setq start
	      (or (previous-single-property-change prev 'fontified nil (point))
		  (point)))))
      ;; Fontify and flag the region as `fontified'.
      (font-lock-after-change-function start end 0)
      (put-text-property start end 'fontified t)
      (or modified (set-buffer-modified-p nil)))))

(defun lazy-lock-fontify-walk-stealthily ()
  ;; Fontify regions in all required buffers while there is no input.
  (let ((buffers (buffer-list)) (continue t) fontified message-log-max)
    (save-excursion
      (while (and buffers continue)
	(set-buffer (car buffers))
	(if (and lazy-lock-mode (lazy-lock-unfontified-p))
	    ;; Fontify regions in this buffer while there is no input.
	    (let ((bufname (buffer-name)))
	      (if (and lazy-lock-stealth-verbose (not fontified))
		  (message "Fontifying stealthily..."))
	      ;; We `save-restriction' and `widen' around everything as
	      ;; `lazy-lock-fontify-stealthily' doesn't and we `sit-for'.
	      (save-restriction (widen) (lazy-lock-fontify-stealthily))
	      (while (and (lazy-lock-unfontified-p)
			  (setq continue (lazy-lock-sit-for
					  lazy-lock-stealth-nice)))
		(if lazy-lock-stealth-verbose
		    (message "Fontifying stealthily... %2d%% of %s"
			     (lazy-lock-percent-fontified) bufname))
		(save-restriction (widen) (lazy-lock-fontify-stealthily)))
	      ;; Note that fontification occurred.
	      (setq fontified t)))
	(setq buffers (cdr buffers))))
    (if (and lazy-lock-stealth-verbose fontified)
	(message "Fontifying stealthily... %s." (if continue "done" "quit")))))

(defun lazy-lock-unfontified-p ()
  ;; Return non-nil if there is anywhere still to be `fontified'.
  (save-restriction
    (widen)
    (text-property-not-all (point-min) (point-max) 'fontified t)))

(defun lazy-lock-percent-fontified ()
  ;; Return the percentage (of characters) of the buffer that are `fontified'.
  (save-restriction
    (widen)
    (let ((size 0) (start (point-min)) (max (point-max)) end)
      (while (setq start (text-property-any start max 'fontified t))
	(setq end (or (text-property-not-all start max 'fontified t) max)
	      size (+ size (- end start))
	      start end))
      ;; Saying "99% done" is probably better than "100% done" when it isn't.
      (truncate (/ (* size 100.0) (buffer-size))))))

(defun lazy-lock-colour-invisible ()
  ;; Fontify the current buffer in `lazy-lock-invisible-face'.
  (save-restriction
    (widen)
    (let ((face 'lazy-lock-invisible-face)
	  (fore (if (stringp lazy-lock-invisible-foreground)
		    lazy-lock-invisible-foreground
		  (cdr (assq 'background-color (frame-parameters)))))
	  (modified (buffer-modified-p)) (inhibit-read-only t)
	  (buffer-undo-list t)
	  deactivate-mark buffer-file-name buffer-file-truename)
      (make-face face)
      (if (not (equal (face-foreground face) fore))
	  (condition-case nil
	      (set-face-foreground face fore)
	    (error (message "Unable to use foreground \"%s\"" fore))))
      (put-text-property (point-min) (point-max) 'face face)
      (put-text-property (point-min) (point-max) 'fontified nil)
      (or modified (set-buffer-modified-p nil)))))

;; Functions for Emacs:

;; This fix is for a number of bugs in the function in Emacs 19.28.
(if (and (not lazy-lock-running-xemacs-p) (< emacs-minor-version 29))
    (defun font-lock-fontify-region (start end &optional loudly)
      "Put proper face on each string and comment between START and END."
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char start)
	  (beginning-of-line)
	  (if loudly (message "Fontifying %s... (syntactically...)" (buffer-name)))
	  (let ((inhibit-read-only t) (buffer-undo-list t)
		buffer-file-name buffer-file-truename
		(modified (buffer-modified-p))
		(old-syntax (syntax-table))
		(synstart (if comment-start-skip
			      (concat "\\s\"\\|" comment-start-skip)
			    "\\s\""))
		(comstart (if comment-start-skip
			      (concat "\\s<\\|" comment-start-skip)
			    "\\s<"))
		(startline (point))
		state prev prevstate)
	    (unwind-protect
		(progn
		  (if font-lock-syntax-table
		      (set-syntax-table font-lock-syntax-table))
		  ;; Find the state at the line-beginning before START.
		  (if (eq startline font-lock-cache-position)
		      (setq state font-lock-cache-state)
		    ;; Find outermost containing sexp.
		    (beginning-of-defun)
		    ;; Find the state at STARTLINE.
		    (while (< (point) startline)
		      (setq state (parse-partial-sexp (point) startline 0)))
		    (setq font-lock-cache-state state
			  font-lock-cache-position (point)))
		  ;; Now find the state precisely at START.
		  (setq state (parse-partial-sexp (point) start nil nil state))
		  ;; If the region starts inside a string, show the extent of it.
		  (if (nth 3 state)
		      (let ((beg (point)))
			(while (and (re-search-forward "\\s\"" end 'move)
				    (nth 3 (parse-partial-sexp beg (point) nil nil
							       state))))
			(put-text-property beg (point) 'face font-lock-string-face)
			(setq state (parse-partial-sexp beg (point)
							nil nil state))))
		  ;; Likewise for a comment.
		  (if (or (nth 4 state) (nth 7 state))
		      (let ((beg (point)))
			(save-restriction
			  (narrow-to-region (point-min) end)
			  (condition-case nil
			      (progn
				(re-search-backward comstart (point-min) 'move)
				(forward-comment 1)
				;; forward-comment skips all whitespace,
				;; so go back to the real end of the comment.
				(skip-chars-backward " \t"))
			    (error (goto-char end))))
			(put-text-property beg (point) 'face
					   font-lock-comment-face)
			(setq state (parse-partial-sexp beg (point)
							nil nil state))))
		  ;; Find each interesting place between here and END.
		  (while (and (< (point) end)
			      (setq prev (point) prevstate state)
			      (re-search-forward synstart end t)
			      (progn
				;; Clear out the fonts of what we skip over.
				(remove-text-properties prev (point) '(face nil))
				;; Verify the state at that place
				;; so we don't get fooled by \" or \;.
				(setq state (parse-partial-sexp prev (point)
								nil nil state))))
		    (let ((here (point)))
		      (if (or (nth 4 state) (nth 7 state))
			  ;; We found a real comment start.
			  (let ((beg (match-beginning 0)))
			    (goto-char beg)
			    (save-restriction
			      (narrow-to-region (point-min) end)
			      (condition-case nil
				  (progn
				    (forward-comment 1)
				    ;; forward-comment skips all whitespace,
				    ;; so go back to the real end of the comment.
				    (skip-chars-backward " \t"))
				(error (goto-char end))))
			    (put-text-property beg (point) 'face
					       font-lock-comment-face)
			    (setq state (parse-partial-sexp here (point)
							    nil nil state)))
			(if (nth 3 state)
			    (let ((beg (match-beginning 0)))
			      (while (and (re-search-forward "\\s\"" end 'move)
					  (nth 3 (parse-partial-sexp
						  here (point) nil nil state))))
			      (put-text-property beg (point) 'face
						 font-lock-string-face)
			      (setq state (parse-partial-sexp here (point)
							      nil nil state))))))
		    ;; Make sure PREV is non-nil after the loop
		    ;; only if it was set on the very last iteration.
		    (setq prev nil)))
	      (set-syntax-table old-syntax)
	      (and prev
		   (remove-text-properties prev end '(face nil)))
	      (and (buffer-modified-p)
		   (not modified)
		   (set-buffer-modified-p nil))))))))

;; Functions for XEmacs:

;; These fix bugs in `text-property-any' and `text-property-not-all'.  They may
;; not work perfectly in 19.11 and below because `next-single-property-change'
;; is also broke and not easily fixable in Lisp.
(if (and lazy-lock-running-xemacs-p (< emacs-minor-version 12))
    (progn
      ;; Loop through property changes until found.  This fix includes a work
      ;; around which prevents a bug in `window-start' causing a barf here.
      (defun text-property-any (start end prop value &optional buffer)
	"Check text from START to END to see if PROP is ever `eq' to VALUE.
If so, return the position of the first character whose PROP is `eq'
to VALUE.  Otherwise return nil."
	(let ((start (min start end)) (end (max start end)))
	  (while (and start (not (eq (get-text-property start prop buffer) value)))
	    (setq start (next-single-property-change start prop buffer end)))
	  start))
      ;; No need to loop here; if it's not at START it's at the next change.
      ;; However, `next-single-property-change' sometimes returns LIMIT, or
      ;; `point-max', if no change is found and sometimes returns nil.
      (defun text-property-not-all (start end prop value &optional buffer)
	"Check text from START to END to see if PROP is ever not `eq' to VALUE.
If so, return the position of the first character whose PROP is not
`eq' to VALUE.  Otherwise, return nil."
	(if (not (eq value (get-text-property start prop buffer)))
	    start
	  (let ((next (next-single-property-change start prop buffer end))
		(end (or end (save-excursion (and buffer (set-buffer buffer))
					     (point-max)))))
	    (and next (< next end) next))))))

;; XEmacs 19.11 function `font-lock-any-extents-p' looks for `text-prop' rather
;; than `face'.  Since `font-lock-unfontify-region' only removes `face', and we
;; have non-font-lock properties hanging about, `text-prop' never gets removed.
;; Unfortunately `font-lock-any-extents-p' is inlined so we can't redefine it.
(if (and lazy-lock-running-xemacs-p (< emacs-minor-version 12))
    (add-hook 'font-lock-mode-hook
     (function (lambda ()
	(remove-hook 'after-change-functions 'font-lock-after-change-function)
	(add-hook 'after-change-functions
	 (function (lambda (beg end old-len)
	    (let ((a-c-beg beg) (a-c-end end))
	      (save-excursion
		;; First set `text-prop' to nil for `font-lock-any-extents-p'.
		(goto-char end) (forward-line 1) (setq end (point))
		(goto-char beg) (beginning-of-line) (setq beg (point))
		(put-text-property beg end 'text-prop nil)
		;; Then do the real `font-lock-after-change-function'.
		(font-lock-after-change-function a-c-beg a-c-end old-len)
		;; Now set `fontified' to t to stop `lazy-lock-fontify-window'.
		(put-text-property beg end 'fontified t))))))))))

(if (and lazy-lock-running-xemacs-p (>= emacs-minor-version 12))
    ;; XEmacs 19.12 font-lock.el's `font-lock-fontify-buffer' runs a hook.
    (add-hook 'font-lock-after-fontify-buffer-hook
	      'lazy-lock-after-fontify-buffer))

;; Cope with the differences between Emacs and [LX]Emacs.
(or (fboundp 'frame-parameters)
    (defalias 'frame-parameters 'screen-parameters))

;; Install ourselves:

;; We don't install ourselves on `font-lock-mode-hook' as other packages can be
;; used with font-lock.el, and lazy-lock.el should be dumpable without forcing
;; people to get lazy or making it difficult for people to use alternatives.
(add-hook 'post-command-hook 'lazy-lock-post-command-fontify-windows)
(add-hook 'post-command-hook 'lazy-lock-post-command-fontify-stealthily t)
(add-hook 'window-setup-hook 'lazy-lock-post-setup-emacs-fontify-windows)
(add-hook 'window-size-change-functions 'lazy-lock-post-resize-fontify-windows)

;; Package-specific.
(add-hook 'ediff-after-setup-control-frame-hooks
	  'lazy-lock-post-setup-ediff-control-frame)

;; Might as well uninstall too.  Package-local symbols would be nice...
(and (fboundp 'unintern) (unintern 'lazy-lock-running-xemacs-p))
(and (fboundp 'unintern) (unintern 'lazy-lock-sit-for))

;; Maybe save on the modeline?
;;(setcdr (assq 'font-lock-mode minor-mode-alist) '(" Lazy"))

(or (assq 'lazy-lock-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(lazy-lock-mode " Lazy") minor-mode-alist)))

;; Provide ourselves:

(provide 'lazy-lock)

;;; lazy-lock.el ends here

