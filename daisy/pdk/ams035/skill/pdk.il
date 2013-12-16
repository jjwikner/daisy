;;(defun daisyPrintf (t_formatString @rest l_args) (apply 'printf (cons (strcat "DAISY:: " t_formatString) l_args)))

(daisyLog " ... loading pdk specific skill ... \n")

(setq daisyilPath 
      (strcat (getShellEnvVar "PDK_HOME") 
	      "/skill/"))

(if (not (boundp 'onlyOcean)) 
    (setq onlyOcean nil))

(setq ilLoadList 
      (list 
       (strcat "cdsinit_" (getShellEnvVar "CDSDATABASE"))
       "pdkEnv" 
       ;;        "libInit" 
       ;;        "daisyLeModsSpec"
       ))

(foreach file 
	 (if onlyOcean 
	     ilLoadList 
	   (append 	
	    ilLoadList 
	    (list 
	     ;;      "libInit"
	     ;; (strcat "cdsinit_" (getShellEnvVar "CDSDATABASE"))
	     ;;	     "daisyMultiPaths" 
	     ;;	     "daisyMGC"
	     ;;      "cyUseful"
	     "daisyBindkeys" 
	     )))
	 
	 (loadi (strcat daisyilPath 
			file 
			".il")))

