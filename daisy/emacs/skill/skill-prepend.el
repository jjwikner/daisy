;; Skill-mode for Skill code programming in GnuEmacs.
;; Copyright (C) 1993,1994,1995 Jonas Jarnestrom.

;; skill-prepend.el,v 1.4 1996/02/09 19:35:24 euajojm Exp
;; Author: Jonas Jarnestrom <etxjojm@eua.ericsson.se>

;; Works for both GnuEmacs 19.2x and Xemacs 19.1x
;; Prepend "site-lisp/skill" to load-path.
;; Autoload skill-mode each time you visit a *.il file.

(setq load-path
      (cons
       (concat (car (delq nil 
			  (mapcar '(lambda (arg) 
				     (and (string-match "site-lisp/?\\'" arg)
					  arg))
				  load-path))) 
	       "/skill") 
       load-path))
    
;; Define where to find skill-mode and autoload by filename extension.

(autoload 'skill-mode "skill-mode" "Skill-mode" t)
(nconc auto-mode-alist '(
                         ("\\.il$"           . skill-mode)
                         ("\\.tf$"           . skill-mode)
                         ("\\.base$"         . skill-mode)
                         ("^.*/\\.cdstool"   . skill-mode)
                         ("^.*/\\.cdsinit.*" . skill-mode)
                         ("^.*/CDSmakefile"  . skill-mode)
			 ))
