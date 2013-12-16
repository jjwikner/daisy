;; This function creates all the required cadence libraries
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

(foreach ddc (reverse ddcList)
	 (foreach tag (list "" "Test")
		  (setq libPath  
			(strcat (getShellEnvVar "PROJAREA")
				"/" ddc "/" 
				(getShellEnvVar "CDSDATABASE") "/" ddc tag))
		  (printf " : %s \n" libPath)

		  ll = (dbCreateLib  (strcat ddc tag) libPath)

		  techSetTechLibName(ll "cdsDefTechLib")
		  
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



