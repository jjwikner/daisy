;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         skill-header.el
; RCS:		$Id: skill-header.el,v 1.12 1995/12/15 17:46:40 euajojm Exp $
; Description:  NMODE style standard headers. 
; Author:       Jerry Duggan, HP/FSD  
; Created:      Thu May 29 11:27:45 1986 
; Modified:     Tue Oct 11 20:12:05 1994 (etxjojm) etxjojm@euah01i1c04
; Language:     Emacs-Lisp
; Package:      
; Status:       Experimental (Do Not Distribute)
;
; (c) Copyright 1986, 1987, 1988, 1989, 1990, 1991, Hewlett-Packard.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer specific variables

(make-variable-buffer-local 'default-header-comment-character)
(make-variable-buffer-local 'header-comment-character)
(make-variable-buffer-local 'header-prefix)
(make-variable-buffer-local 'header-suffix)

(set-default 'default-header-comment-character ?\;)
(set-default 'header-comment-character ?\;)
(set-default 'header-prefix nil)
(set-default 'header-suffix nil)

;; Nice defvars for various globals

(defvar buffer-header-defaults
  '((lisp-mode             . (?\; "; -*-Lisp-*-" nil))
    (skill-mode            . (?\; nil nil))
    (lisp-interaction-mode . (?\; "; -*-Emacs-Lisp-*-" nil))
    (emacs-lisp-mode       . (?\; "; -*-Emacs-Lisp-*-" nil))
    (scheme-mode           . (?\; "; -*-Scheme-*-" nil))
    (c-mode                . (?\* "/* -*-C-*-" "*/"))
    (plain-TeX-mode        . (?\% "% -*-TeX-*-" nil))
    (plain-tex-mode        . (?\% "% -*-TeX-*-" nil))
    (TeX-default-mode      . (?\% "% -*-TeX-*-" nil))
    (LaTeX-mode            . (?\% "% -*-LaTeX-*-" nil))
    (latex-mode            . (?\% "% -*-LaTeX-*-" nil)))
  "*Alist of default variables for header stuff.  Each element is of the form:
  (<mode> . (<default> <prefix> <suffix>))
    <mode> is the mode of the buffer (lisp-mode, c-mode, etc).
    <default> is the default header comment character.
    <prefix> is the header-prefix.
    <suffix> is the header-suffix.")
		  
;;; General predicates for lines and headers

; Returns T if LINE is a marker line
(defun header-marker-line-p (line)
  (save-excursion
    (if (eq (goto-line line) 0)
	(if (let ((p-c (preceding-char)))
	      (or (= p-c 0)
		  (= p-c ?\n)))
	    (let* ((pt (point))
		   (pt80 (+ pt 79))
		   (tmp-h-c-c nil))
	      (if (< pt80 (point-max))
		(let* ((str (buffer-substring pt pt80))
		       (cmp-str
			(make-string
			 79
			 (setq tmp-h-c-c
			       (or
				header-comment-character
				(string-to-char str))))))
		  (setq pt-80 cmp-str)
		  (and
		   (string-equal str cmp-str)
		   (setq header-comment-character tmp-h-c-c)))
		nil))
	  nil)
      'error)
    ))

; Returns T if line is a blank comment line

(defun header-blank-comment-line-p (line)
  (save-excursion
    (if (eq (goto-line line) 0)
      (and (let ((p-c (preceding-char)))
	     (or (= p-c 0)
		 (= p-c ?\n)))
	   (= (char-after (point)) header-comment-character)
	   )
      'error)))

; Is true if the buffer has a standard header

;;;###autoload   
(defun buffer-has-header-p ()
  (save-excursion
    (let ((header-start (find-first-header-line)))
      (if header-start
	(let* ((string-begin
		(progn
		  (goto-line (+ header-start 2))
		  (point)))
	       (string-end (+ string-begin 79)))
	  (string-match "File:"
			(buffer-substring string-begin string-end)))
	(find-last-header-line)
	(progn
	  (setq header-comment-character default-header-comment-character)
	  nil)))))

;;; Locators for first, last header line

; Returns the line number of the first header line

;;;###autoload
(defun find-first-header-line ()
  (catch 'header
    (let ((i 1))
      (while (< i il-header-search-limit)
	(let ((h-m-l (header-marker-line-p i)))
	  (cond ((eq h-m-l 'error)
		 (throw 'header nil))
		((and h-m-l
		      (save-excursion
			(goto-line (1+ i))
			(eq (char-after (point)) header-comment-character)))
		 (throw 'header i))
		(t (setq i (1+ i))))))
      nil)))

; Returns the line number of the last header line

;;;###autoload
(defun find-last-header-line ()
  (catch 'header
    (let ((f-h-l (find-first-header-line)))
      (if f-h-l
	(let ((i (1+ f-h-l))
	      (max-search-line (+ f-h-l il-header-max-size)))
	  (while (< i max-search-line)
	    (let ((h-m-l (header-marker-line-p i)))
	      (cond ((eq h-m-l 'error)
		     (throw 'header nil))
		    (h-m-l
		     (throw 'header i))
		    (t (setq i (1+ i))))))
	  nil)
	nil))))

;;; utility functions

(defun line-to-char-offset (line)
  (and (number-or-marker-p line)
       (goto-line line))
  (point))

;;; Make-Header cookies

(defun insert-attribute-line (cchar attr spaces value)
  (insert-string (format "%c %s: %s%s\n" cchar attr spaces value)))

;;;###autoload
(defun il-make-file-header ()
  "Makes a header in the current buffer, if there is none already there"
  ;;(interactive)
  (if (buffer-has-header-p)
    (error "file already has standard header")
    (let ((header-string (make-string 80 header-comment-character))
	  (blank-string (make-string 2 ?\ ))
	  old-point)
      (aset header-string 79 ?\n)
      (aset blank-string 0 header-comment-character)
      (aset blank-string 1 ?\n)
      (goto-line 1)
      (setq old-point (point))
      (if header-prefix
	(progn
	  (insert-string header-prefix)
	  (insert-string "\n")))
      (insert-string header-string)
      (insert-string blank-string)
      (insert-attribute-line header-comment-character "File" "        "
			      (file-name-nondirectory (or
						       (buffer-file-name)
						       "")))
      (and il-header-VCS
	   (insert-attribute-line header-comment-character 
				  il-header-VCS "         "
				  il-header-VCS-header-keyw))
      (insert (format "%c\n" header-comment-character))
      (insert-attribute-line header-comment-character "Description" " " "")
      (insert-attribute-line header-comment-character "Author" "      "
			     (or il-header-author-name
				 il-header-user-name
				 ""))
      (insert-attribute-line header-comment-character "Created" "     "
			     (il-current-time-string))
      (insert-attribute-line header-comment-character "Modified" "    " "")
      (insert-attribute-line header-comment-character "Language" "    "
			     (if (string= mode-name "Fundamental")
				 il-not-applicable
			       mode-name))
      (insert-attribute-line header-comment-character "Package" "     " 
			     il-package-prefix)
      (insert-attribute-line header-comment-character "MainFun" "     " 
			     (setq il-main-fun (il-query-main-fun)))
      (insert-attribute-line header-comment-character "Status" "      "
			     "Experimental (Do Not Distribute)")
      (insert-string blank-string)
      (insert-string (format "%c (C) Copyright %s, %s%s\n"
			     header-comment-character
			     (substring (current-time-string) -4)
			     (or il-header-copyright-notice
				 il-header-user-company-name
				 il-header-user-name
				 " ")
			     (if il-header-copyright-notice
			       ""
			       ", all rights reserved.")))
      (insert-string blank-string)
      (insert-string header-string)
      (if header-suffix
	(progn
	  (insert-string header-suffix)
	  (insert-string "\n")))
      ;;(insert-string "\n")
      (goto-char (string-match "Description"
			       (buffer-substring old-point (point))))
      (end-of-line)
      )))

; Update the modify time of a standard header

;;;###autoload   
(defun il-header-update-modify-time ()
  (il-read-local-vars (point) nil)
  (unwind-protect
  (if (and (buffer-has-header-p) il-timestamp-file-header)
    (save-excursion
      (setq il-prev-sess-date (substring (il-current-time-string) 0 6))	; update prev-sess-date.
      (let ((header-begin nil)
	    (header-end nil)
            (mod-field nil)
	    (modify-position nil))
      (setq header-begin (line-to-char-offset (find-first-header-line)))
      (setq header-end   (line-to-char-offset (find-last-header-line)))
      (if (and header-begin header-end)
        (progn
	  (setq mod-field  (string-match (format "%c Modified:" header-comment-character)
					     (buffer-substring header-begin
							       header-end)))
	  (if (and mod-field (< header-begin mod-field)(< mod-field header-end))
             (progn
	       (setq modify-position
		     (+ mod-field (1- header-begin)))
	       (if modify-position
		   (progn
		     (goto-char (- modify-position 10))
		     (if (re-search-forward " Modified:.*\n" nil t)
			 (replace-match 
			  (format " Modified:     %s\n"
				  (if il-header-user-name
				      (format "%s %s %s"
					      (il-current-time-string)
					      (or il-block-name
						  (marker-position 
						   il-this-sess-last-edit))
					      il-header-user-name)
				    (il-current-time-string))) t t)))
		 nil))
	    nil))
	nil)))))
 nil)
    
;;; Make-Revision cookies

; Will position to a newly made revision entry, making the history if necessary

(defun header-find-or-make-revision-history (VCS-keyw)
  (if (buffer-has-header-p)
    (let ((last-line (find-last-header-line))
	  (revision-string (format "%c %s\n"
				   header-comment-character
				   (or VCS-keyw il-header-revision-headline))))
      (if (and (header-blank-comment-line-p (1+ last-line))
	       (header-blank-comment-line-p (+ last-line 3))
	       (string-equal revision-string
			     (buffer-substring
			      (line-to-char-offset (+ last-line 2))
			      (line-to-char-offset (+ last-line 3)))))
	(goto-line (+ last-line 4))
	(let ((header-string (make-string 80 header-comment-character))
	      (blank-string (make-string 2 ?\ )))
	  (aset header-string 79 ?\n)
	  (aset blank-string 0 header-comment-character)
	  (aset blank-string 1 ?\n)
	  (goto-line (+ last-line 1))
	  (insert-string blank-string)
	  (insert-string revision-string)
	  (insert-string blank-string)
	  (insert-string header-string)
	  (goto-line (+ last-line 4)))))
    (error "buffer does not have a standard header")))

;;;###autoload
(defun il-create-revision ()
  "Make a revision entry, creating the revision header if needed.
As a prerequisite, buffer must have a standard file header.
If il-header-VCS and il-header-VCS-history-keyw are set, the VCS will
control the header once it has been created.
Admin-options: il-header-VCS,il-header-VCS-history-keyw
"
  (interactive)
  (header-find-or-make-revision-history 
   (and il-header-VCS il-header-VCS-history-keyw))
  (if (and il-header-VCS il-header-VCS-history-keyw)
      nil
    (insert-string (format "%c %s (%s) %s@%s\n%c  \n"
			   header-comment-character
			   (il-current-time-string)
			   (or il-header-user-name
			       "")
			   (user-login-name)
			   (system-name)
			   header-comment-character))
    (if auto-fill-function
	(progn
	  (setq fill-prefix (format "%c  " header-comment-character))
	  (message "fill prefix is now '%s'" fill-prefix))
      nil)
    (backward-char)
    (message "Please enter revision information.")))

; language customizations:

;;;###autoload
(defun set-buffer-header-vars ()
  (let ((mode-vars
	 (cdr (assoc (setq mm major-mode) buffer-header-defaults))))
    (if mode-vars
	(setq default-header-comment-character (car mode-vars)
	      header-prefix                    (car (cdr mode-vars))
	      header-suffix                    (car (cdr (cdr mode-vars))))
      (setq header-comment-character default-header-comment-character))))

(defun my-init ()
  (set-buffer-header-vars)
  (if (and (not (file-exists-p buffer-file-name)) (not (buffer-has-header-p)))
      (make-header)))
      
;;;###autoload
(defun get-header-info ()
  "Return a list of attribute-value pairs from the current buffer's file
header, if it has a header."
  (interactive)
  (if (buffer-has-header-p)
    (save-excursion
      (let ((header-end (line-to-char-offset (find-last-header-line)))
	    (attr nil)
	    (val nil)
	    (pairs nil))
	(goto-line (find-first-header-line))
	(while (and (< (point) header-end)
	            (re-search-forward (format "^%c [A-Z][a-z]*:" header-comment-character)
			               header-end t))
	  (setq attr (upcase (buffer-substring (+ 2 (match-beginning 0))
					       (1- (match-end 0)))))
	  (goto-char (1+ (match-end 0)))
	  (if (re-search-forward "[^ 	].*$" (point-at-end-of-line) t)
	    (setq val (buffer-substring (match-beginning 0) (match-end 0)))
	    (setq val nil))
	  (setq pairs (cons (cons attr val) pairs))
	  (forward-line 1)
	  )
	pairs))))

(defun point-at-end-of-line ()
  "Returns the value of point at the end of the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (point)))

