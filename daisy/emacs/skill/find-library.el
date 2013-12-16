(define-key global-map "\C-x\C-l" 'er-find-library)

(defun er-find-library (library)	;from Barry Warsaw
  "Find and edit the source for elisp LIBRARY.
Searching the dirs of load-path."
  (interactive "sFind library: ")
  (let* ((elcfile (locate-library library))
	 (elfile (and elcfile
		      (if (string-match "\\.elc" elcfile)
			  (substring elcfile 0 (1- (length elcfile)))
			elcfile))))
    (or (and elfile
	     (file-exists-p elfile))
	(error "Library %s not found" library))
    (find-file elfile)))
