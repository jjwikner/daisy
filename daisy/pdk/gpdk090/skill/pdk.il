(printf " ... loading pdk specific skill ... \n")

(setq daisyilPath (strcat (getShellEnvVar "WORKAREA") 
			  "/daisy/pdkSpecific/" 
			  (getShellEnvVar "CDSPROCESSNAME") 
			  "/skill/"))

(if (not (boundp 'onlyOcean)) 
    (setq onlyOcean nil))

(setq ilLoadList 
      (list 
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
	    "libInit"
;;	     "daisyMultiPaths" 
;;	     "daisyMGC"
	     "cyUseful"
	     "daisyBindkeys" 
	     )))
	 
	 (loadi (strcat daisyilPath 
			file 
			".il")))
