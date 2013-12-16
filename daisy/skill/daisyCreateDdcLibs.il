;; This function creates all the required cadence libraries

;; Mapping from daisy process names to technology library strings for library
;; creation.
(setq techlibMap '(
;;
  ( "ams035"          "TECH_C35B4" )
  ( "general"         "" )
  ( "gpdk045"         "gpdk045" )
  ( "gpdk090"         "gpdk090" )
  ( "stm065rf"        "cmos065" )
  ( "stm065rf_536"        "cmos065" )
  ( "stm040rf"        "cmos040" )
  ( "umc018"          "umc18mmrf" )))

(setq ddcList (list))

(if (isFile 
     (strcat 
      (getShellEnvVar "PROJAREA")		   
      "/daisyProjSetup/info/daisyDdcs.txt"))

    (progn
      fid = (infile 
	     (strcat 
	      (getShellEnvVar "PROJAREA")   
	      "/daisyProjSetup/info/daisyDdcs.txt"))

      (while 	(fscanf fid "%s" ddc)
	(printf "%s \n" ddc)
	(setq ddcList (append (list ddc) ddcList))
	)      
      (close fid))
  
  (printf "==> Warning: No daisyDdcs.txt found \n")
  )

sheetId = (dbOpenCellViewByType "daisy" "daisySheet" "symbol")

(foreach ddc ddcList
	 (foreach tag (list "" "Test")
		  (setq libPath  
			(strcat (getShellEnvVar "PROJAREA")
				"/" ddc "/" 
				(getShellEnvVar "CDSDATABASE") "/" ddc tag))
		  (printf " : %s \n" libPath)

		  ll = (dbCreateLib  (strcat ddc tag) libPath)
		  techSetTechLibName(ll (cadr (assoc (getShellEnvVar "CDSPROCESSNAME") techlibMap)))
		  
		  aa = (dbOpenCellViewByType (strcat ddc tag) 
					     (strcat ddc tag "Top" (if (equal tag "Test") "_T" "")) 
					     "schematic" "schematic" "w")

		  (dbCreateInst aa sheetId "sheet" (list 0.0 0.0) "R0")

		  (dbCheck aa)
		  (dbSave aa)
		  (dbClose aa)
		  (csh           (strcat 
		       "echo 'DEFINE "
		       ddc tag " "
		       ddc tag
		       "' >! "	
		       (strcat (getShellEnvVar "PROJAREA")
			       "/" ddc "/" 
			       (getShellEnvVar "CDSDATABASE") "/"
			       "cds." (if (equal tag "Test") "test" "") 
			       "lib")))
		  ))



