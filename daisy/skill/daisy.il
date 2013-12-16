;; Daisy specifics

;; Define daisyLog, a wrapper for printf to provide consistent logging.
;; Currently it prefixes the string 'DAISY:: ' to the message.
(defun daisyLog (t_formatString @rest l_args) (apply 'printf (cons (strcat "DAISY:: " t_formatString) l_args)))

(printf "DAISY:: ... loading %s.il ...\n" "daisy")

(setq daisyilPath 
      (strcat 
       (getShellEnvVar "DAISYAREA") 
       "/skill/"))
(setSkillPath (cons daisyilPath (getSkillPath)))

(if (not (boundp 'onlyOcean))
    (setq onlyOcean nil))

(setq ilLoadList 
      (list 
       "ribeCheckSaveAll"
       "daisyAsiMods"     
       "daisyFindCellInfo"
       "daisyConstants"   
       "daisyListMods"  
       "mathMods"    
       "pwlGen" 
       "awvMods"     
       "fileMods"    
       "daisyMail"   
       "daisyPlot" 
       "daisyMods"
       "pwlGen" 
       "daisyEnv" ))

(foreach file (if onlyOcean 
		  ilLoadList 
		(append ilLoadList 
			(list "daisyLayoutMods" 
			      ;; "cyUseful"       
			      "calcSpecFnsHI"  
			      "CICaddprCellTypeProp"
			      "ccsAbCdfUpdate"
			      "daisySch"
			      "calcSpecFns" 
			      "daisyBindkeys"
			      "daisyOpt"
			      "daisyMgc"
			      "daisyRegs")))
	 (loadi (strcat daisyilPath 
			(strcat file ".il"))))

;; Some launch functions
;; Load file for ocean scrips
;; Should be moved.

(defun lof ( fileName )		
  ;; Load ocean file
  (let ( (userArea (getShellEnvVar "USERAREA")) 
	 (unitPath "/sim/")
	 (oceanPath "/ocean/") 
	 (fullPath nil) )
    (setq fullPath (strcat userArea unitPath oceanPath))
    
    (load (strcat fullPath "/" fileName ".ocn"))))

(if (not (boundp 'onlyOcean)) 
    (setq onlyOcean nil))

;; PROCESS SPECIFIC SKILL FILES

(loadi (strcat (getShellEnvVar "DAISYAREA") 
	       "/pdkSpecific/" 
	       (getShellEnvVar "CDSPROCESSNAME")
	       "/skill/pdk.il"))

;; PROJECT SPECIFIC SKILL FILES

(if (isFile (strcat 	
	     (getShellEnvVar "PROJAREA")
	     "/daisyProjSetup/skill/daisy.il"))    
    (progn
      (loadi (strcat 
	      (getShellEnvVar "PROJAREA")
	      "/daisyProjSetup/skill/daisy.il"))
      
      (setSkillPath (cons  
		     (strcat  
		      (getShellEnvVar "PROJAREA")
		      "/daisyProjSetup/skill/")
		     getSkillPath() ))
      ))

;; DDC SPECIFIC SKILL FILES <== the order 
;; here might need a bit of rethinking. Should we load 
;; daisyProjSetup before or after?


(if (isFile 
     (strcat (getShellEnvVar "PROJAREA")		   
	     "/daisyProjSetup/info/daisyDdcs.txt"))
    (progn
      fid = (infile (strcat (getShellEnvVar "PROJAREA")   
			    "/daisyProjSetup/info/daisyDdcs.txt"))
      (while 	(fscanf fid "%s" ddc);      
	
	(if (isFile (strcat 
		     (getShellEnvVar "PROJAREA")
		     "/" ddc "/"
		     "/skill/daisy.il"))
	    (progn
	      (loadi (strcat  (getShellEnvVar "PROJAREA")
			      "/" ddc "/"
			      "/skill/daisy.il"))
	      (setSkillPath (cons  (strcat  (getShellEnvVar "PROJAREA")
			      "/" ddc "/"
			      "/skill/") getSkillPath() ))
	      )
	  )
	)
      (close fid)
      )
  (printf "DAISY:: ==> Warning: No daisyDdcs.txt found!\n")
  )

;; Mentor ADMS

(let 
    (t_amsPath t_file)
  (setq t_amsPath (getShellEnvVar "MGC_AMS_HOME"))
  (when t_amsPath
    (printf "DAISY:: Loading the Mentor ADMS stuff \n")
    (setq t_file 
	  (strcat t_amsPath "/etc/cds/.cdsinit"))
    (when (isFile t_file) 
      (load t_file))))

	       
(if (and (not onlyOcean) (isCallable 'LMstartTimers))
    (LMstartTimers))

