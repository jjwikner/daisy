(printf "... load daisyHierCopyPrep.il ... \n")

(defun daisyReturnHierPath (stopLibList @optional (cv nil) )
  ;; JJ Wikner

  (let ((cellList nil) (newCell nil ))
    (if (not cv)
	(setq cv (hiGetCurrentWindow)->cellView))
    
    ;; There is a flaw here, nil could be passed on to the function and 
    ;; no active window.
    
    ;; Append the cell information to the list of all cells in the hierarchy
    (setq cellList 
	  (append cellList 
		  (list (list cv->libName  
			      cv->cellName 
			      cv->viewName 
			      cv->cell->writePath))))
    
    ;; Browse through all the instance masters (unique list) in the current view)
    ;; and re-call the function.
    (foreach inst (setof x cv->instanceMasters (not (member x->libName stopLibList)))
	     (if (equal inst->viewName "symbol")
		 (setq newCell (dbOpenCellViewByType 
				inst->libName 
				inst->cellName
				"schematic"))	       
	       (setq newCell inst))
	     
	     (if newCell
		 (setq cellList 
		       (append cellList 
			       (daisyReturnHierPath
				stopLibList 
				newCell )))
	       (setq cellList 
		     (append cellList (list (list inst->libName  
						  inst->cellName 
						  inst->viewName 
						  inst->cell->writePath)))))
	     )
    cellList))


(defun daisyReturnHierLibs (stopLibList @optional (cv nil) )
  ;; JJ Wikner
  ;; Daisy Semiconductor AB

  (let ((cellList nil))
    (if (not cv)
	(setq cv (hiGetCurrentWindow)->cellView))
    
    ;; There is a flaw here, nil could be passed on to the function and 
    ;; no active window. Move (hiGetCurrentWindow)->cellView) to optional
    ;; field instead...
    
    ;; Append the cell information to the list of all cells in the hierarchy
    (if (not (member cv->libName cellList))
	(setq cellList 
	      (append cellList 
		      (list cv->libName ))))
    
    ;; Browse through all the instance masters (unique list) in the current view)
    ;; and re-call the function.
   (foreach inst (setof x cv->instanceMasters (not (member x->libName stopLibList)))
	     (if (equal inst->viewName "symbol")
		 (setq newCell (dbOpenCellViewByType 
				inst->libName 
				inst->cellName
				"schematic"))
	       (setq newCell inst))
	     
	     (if newCell
		 (setq cellList 
		       (append cellList 
			       (daisyReturnHierLibs
				stopLibList 
				newCell))))
	     )
    cellList))

(defun daisyExtractDdcInfo (cellPath projToken)

  ;; e.g. cellPath = "/home/proj/ADvanceII/daisyReg/cds/daisyReg/daisyRegEmptyTop"
  ;; projToken = "/home/proj/ADvanceII"
  ;; return daisyReg

  ;; Crude and ugly:

  (parseString cellPath "/")
  (parseString projToken "/")
  
  (nth (length (parseString projToken "/"))
       (parseString cellPath "/")))


  
(defun daisyReturnHierDdcs (stopLibList projToken @optional (cv nil) )
  ;; JJ Wikner
  ;; Daisy Semiconductor AB

  (let ((cellList nil) (ddc "") )
    (if (not cv)
	(setq cv (hiGetCurrentWindow)->cellView))
    
    ;; There is a flaw here, nil could be passed on to the function and 
    ;; no active window. Move (hiGetCurrentWindow)->cellView) to optional
    ;; field instead...

    (if cv->cell->writePath
	(setq ddc (daisyExtractDdcInfo cv->cell->writePath projToken)))
    
    (if (not (member ddc cellList))
	(setq cellList 
	      (append cellList (list ddc))))
    
    ;; Browse through all the instance masters (unique list) in the current view)
    ;; and re-call the function.
    (foreach inst (setof x cv->instanceMasters (not (member x->libName stopLibList)))
	     (if (equal inst->viewName "symbol")
		 (setq newCell (dbOpenCellViewByType 
				inst->libName 
				inst->cellName
				"schematic"))
	       (setq newCell inst))
	     
	     (if newCell
		 (setq cellList 
		       (append cellList 
			       (daisyReturnHierDdcs
				stopLibList projToken
				newCell))))
	     )
    cellList))

(defun daisyDumpHierInfo 
  (topLibName topCellName 
	      @key
	      (topViewNames (list "layout" "schematic"))
	      (hierStopLibs 
	       (list 
		"basic" "analogLib" )))
  
  (let (( outFilePort nil) (hierList nil) )
    
    (setq outFilePort (outfile (strcat topLibName "." 
				       topCellName
				       ".hier.txt") "w"))
    
    (foreach topViewName topViewNames
	     (setq hierList
		   (daisyReturnHierPath
		    hierStopLibs
		    (dbOpenCellViewByType 
		     topLibName 
		     topCellName 
		     topViewName)))
    
	     (foreach x hierList
		      (when (cadr (cddr x))
			(fprintf outFilePort "%s/%s\n%s/%s\n"  
				 (cadr (cddr x)) (caddr x) 
				 (cadr (cddr x)) "prop.xx")
			(if (equal topViewName "schematic")	     
			    (fprintf outFilePort "%s/%s\n"  
				     (cadr (cddr x)) "symbol" ))
			)))
    (close outFilePort) ))

(defun daisyUniqueList (inputList)
  (let ( ( outputList nil ))
    (foreach element inputList 
	     (if (not (member element outputList))
		 (setq outputList (append outputList (list element)))))
    outputList))

stopLibs = (infile 
	    (strcat (getShellEnvVar "PDK_HOME") 
		    "/pdk/streamRefLibs.txt"))

(gets hierStopLibsTxt stopLibs)
(close stopLibs)

(setq hierStopLibs 
      (append (list "basic" "analogLib" "cmos065" "daisy" "ahdlLib")
	      (parseString hierStopLibsTxt)))

(daisyDumpHierInfo 
 (setq topLibName "xStuckVcoAdc") 
 (setq topCellName "vcoAdcTop")
 ?topViewNames (list "schematic")
 ?hierStopLibs hierStopLibs)


(setq hierLibList 
      (daisyUniqueList 
       (append (daisyReturnHierLibs
		hierStopLibs
		(dbOpenCellViewByType 
		 topLibName 
		 topCellName 
		 "layout"))
	       (daisyReturnHierLibs
		hierStopLibs
		(dbOpenCellViewByType 
		 topLibName 
		 topCellName 
		 "layout")))))

(setq 
 hierDdcList
 (daisyUniqueList 
  (append
   (daisyReturnHierDdcs
    hierStopLibs 
    (strcat (getShellEnvVar "PROJAREA") "/") 
    (dbOpenCellViewByType topLibName topCellName "schematic"))
   (daisyReturnHierDdcs
    hierStopLibs 
    (strcat (getShellEnvVar "PROJAREA") "/") 
    (dbOpenCellViewByType topLibName topCellName "layout")))))

