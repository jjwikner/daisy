;; DEF LEF generation

(printf " ... loading daisyDefLef.il ... \n")

(defun daisyLefOut (@key (cvList nil) (fileName nil) (techLib nil) (outputDir nil))
;; Has to be abstract!
  (let ( ( tempFileForLef "./log/lefout.lst" ))
    
    (if (not cvList) 
	(setq cvList 
	      (list 
	       (list 
		(hiGetCurrentWindow)->cellView->libName
		(hiGetCurrentWindow)->cellView->cellName
		(hiGetCurrentWindow)->cellView->viewName))))
    
    (if (not fileName) (setq fileName (cadar cvList)))
    
    (setq fileName (strcat (if outputDir outputDir "") fileName ".lef"))
    
    (sh (sprintf nil "rm %s" tempFileForLef))
    (sh (sprintf nil "touch %s" tempFileForLef))
    (foreach cv cvList
	     (sh (sprintf nil "echo \"%s %s %s\" >> %s" (car cv) (cadr cv) (caddr cv) tempFileForLef)))
    (printf "------------ %s \n" fileName)
    (ldtrLefWrite 
     fileName
     (if techLib techLib "tsmcN80")
     t
     tempFileForLef
     )))


(defun daisyDefOut (@optional (cv nil) (outputDir nil))
  (let ((fileName ""))

    (if (not cv) 
	(setq cv (hiGetCurrentWindow)->cellView))  
    
    (setq dfIILEFDEFCurrentVersion "5.5")  
    
    (ldtrDefWrite
     (setq fileName (strcat (if outputDir outputDir "") cv->cellName ".def"))
     cv->libName
     cv->cellName
     cv->viewName
     "SE"
     t
     ) 
    
    (sh 
     (sprintf nil "cat %s | sed 's/|/\\//g' | sed 's/</\\[/g' | sed 's/>/\\]/g' > %s.mod " 
	      fileName fileName
	      ))
    )
  )


(defun daisyFlattenCell (@optional (cv nil) (listOfInst nil))
  (if (not cv) 
      (setq cv (hiGetCurrentWindow)->cellView))
  (if (not listOfInst)
      (setq listOfInst cv->instances))
  (foreach inst listOfInst
	   (leFlattenInst 
	    (if inst->mosaic inst->mosaic inst)
	    32 t nil nil )
	   ;; For some reason the pins are brought up through hierarchy...
	   ;; Needs to be taken care of later on.
	   )
  )

(defun daisyRemoveInvalidLayers (lppList @optional (cv nil) (listOfShapes nil))
  (if (not cv) 
      (setq cv (hiGetCurrentWindow)->cellView))
  (if (not listOfShapes) 
      (setq listOfShapes cv->shapes))
  ;; Clean it up
  (foreach shape listOfShapes
	   (if (not (member shape->lpp listOfLayersToSave))
	       (dbDeleteObject shape)
	     ))
  )

(defun daisyConvertPurpose (oldPurpose newPurpose @optional (cv nil) (listOfShapes nil))
  (if (not cv) 
      (setq cv (hiGetCurrentWindow)->cellView))
  (if (not listOfShapes) 
      (setq listOfShapes cv->shapes))
  
  (foreach shape listOfShapes 
	   (if (equal shape->purpose oldPurpose)
	       shape->purpose=newPurpose
	     )))

(defun daisyCreateTempLayout (@optional (cv nil) (tag "Lef") (newLibName nil))
  ;; Just a copy

  (if (not cv) 
      (setq cv (hiGetCurrentWindow)->cellView))
  
  (dbSave cv 
	  (if (not newLibName) cv->libName newLibName)
	  cv->cellName 
	  (strcat "layout" tag))
  )

(defun daisyFlattenCellForLef (@optional (cv nil) (newLibName nil)  )
  (let ( ( listOfMetalsToSave nil )
	 ( listOfLayersToSave nil ) 
	 ( tag "" ) (origCv nil) )
    
    (if (not cv) 
	(setq cv (hiGetCurrentWindow)->cellView))
    
    ;; Create a new layout
    (daisyCreateTempLayout cv (setq tag (makeTempFileName "Lef")))
    
    ;; Save the pointer to original file
    (setq origCv cv)
    
    ;; Point to the dummy copy
    (setq cv
	  (dbOpenCellViewByType 
	   cv->libName 
	   cv->cellName 
	   (strcat "layout" tag )
	   "" "a"
	   ))
    
    ;; Generate the layers that should be kept in the design
    (foreach validLayer (list "M2" "M3" "M4" "M5" "M6" )
	     (foreach validPurpose (list "drawing" "boundary") ;;  Pins are preserved for some reason ... "pin")
		      (setq listOfMetalsToSave
			    (append (list (list validLayer validPurpose))
				    listOfMetalsToSave
				    ))))
    
    (setq listOfLayersToSave
	  (append (list 
		   (list "prBoundary" "drawing")
		   (list "prBoundary" "boundary")
		   )
		  listOfMetalsToSave))
    
    ;, Flatten the copied view
    (daisyFlattenCell cv)
    
    (daisyRemoveInvalidLayers listOfLayersToSave cv)    

    (foreach figId (setof x cv->shapes (and (equal x->objType "label")
					    (equal x->layerName "prBoundary")))
	     (if figId (dbDeleteObject figId)))
    
    
    (daisyConvertPurpose "drawing" "boundary" cv 
			 (setof x cv->shapes (assoc x->layerName listOfMetalsToSave)))
    
    (leMergeShapes cv->shapes)
    
    (foreach pin (setof x origCv->shapes (equal x->purpose "pin"))
	     (dbCopyFig pin cv))
    
    (daisyPinThemDown nil cv)
    (dbSave cv)
    (dbClose cv)
    
    ))


(defun daisyReturnHierarchy (termLibList @optional (cv nil) )
  (let ((cellList nil))
    (if (not cv)
	(setq cv (hiGetCurrentWindow)->cellView))

    (setq cellList (append cellList (list (list cv->libName  cv->cellName cv->viewName))))

    (foreach inst (setof x cv->instanceMasters (not (member x->libName termLibList)))
	     ;;(setq cellList (append cellList (list (list inst->libName  inst->cellName inst->viewName))))		   
	     (setq cellList (append cellList (daisyReturnHierarchy termLibList inst)) )
	     )    
    cellList)  
  )


(defun daisyCopyCells (listOfCells targetLib @optional (tag ""))
  ;; Assuming a list of format (list (list libName cellName viewName) ... )
  (let ((newListOfCells nil))
    
    (foreach cv listOfCells
	     (dbSave 
	      (dbOpenCellViewByType 
	       (car cv) (cadr cv) (caddr cv) "maskLayout" "r")
	      targetLib
	      (cadr cv)
	      (strcat (caddr cv) tag)) 
	     (setq newListOfCells 
		   (append 
		    (list (list targetLib (cadr cv) (strcat (caddr cv) tag)))
		    listOfCells))
			   
	     )
    newListOfCells)
  )


(defun daisyDefLeffard (listOfCells @key (localLib nil) (localTag "") (outputDir nil) (techLib "tsmcN80LefExport") (lefFileName nil))
;; We can only write abstract/lef if we have localLib.  
  (let ( (lefList nil) )

    ;; debug -- ok
    (setq newListOfCells 
	  (if localLib
	      (daisyCopyCells listOfCells localLib localTag)
	    listOfCells))
    
    (foreach cv (reverse newListOfCells)

	     (daisyFlattenCellForLef 
	      (setq cvId 
		    (dbOpenCellViewByType
		     (if localLib localLib (car cv))
		     (cadr cv)
		     (strcat (caddr cv) localTag) 
		     "" "a"))	  
	      localLib)

	     (if localLib
		 (daisyCreateEncounterStrips 
		  (dbOpenCellViewByType
		   localLib
		   (cadr cv)
		   (strcat (caddr cv) localTag) 
		   "" "a")))
	     
	     (daisyDefOut cvId (if outputDir (strcat outputDir "/def/") ""))
	     (dbSave cvId)	
	     ;; (if localLib (dbSave cvId localLib (cadr cv) "abstract"))
	     (if localLib (asSoftBlockCreateAbstract cvId "abstract"))
	     (dbClose cvId)
	     (if localLib
		 (setq lefList (append 
				(list (list localLib (cadr cv) "abstract"))
				lefList)))	       
	     )
    (if localLib
	;; Strcat is wrong here ?
	(daisyLefOut 
	 ?cvList lefList 
	 ?techLib techLib
	 ?outputDir (if outputDir (strcat outputDir "/lef/") "")
	 ?fileName lefFileName)
      )
))

(setq cvList nil)

(foreach  cv (list "daisyHdmiPhaseSel" "daisyHdmiHandoverHS" "daisyHdmiDataChannelDig")
	  (setq cvId 
		(dbOpenCellViewByType 
		 "daisyHdmiDualPhy"
		 cv
		 "layout"
		 "maskLayout" "r"))
	  (setq cvList 
		(append
		 (daisyReturnHierarchy 
		  (list "daisyHdmiDualPhy_tech" 
			"daisyHdmiDualPhyFill"
			"tcbn90ghp_150a" 
			"tcbn90ghp_150ab" 
			"tcbn90ghphvt_150a" 
			"tsmcN80" 
			"tsmcN80LefExport")
		  (dbOpenCellViewByType 
		   "daisyHdmiDualPhy"
		   cv
		   "layout"
		   "maskLayout" "r"))
		 cvList)))

(defun daisyWriteLefList (cvList)
  (let ((fileName "lefout.lst")
	(fileport nil))

    (setq fileport (outfile (strcat (getShellEnvVar "USERAREA") "/log/" fileName) "w" ))
    
    (foreach cv cvList 
	     (printf   "work_jacobw_defLef %s abstract\n" (cadr cv))
	     (fprintf fileport  "work_jawi_defLef %s abstract\n" (cadr cv)))
    (close fileport)))
	   


(daisyDefLeffard 
 cvList 
 ?localLib "work_jawi_defLef"
 ?localTag "DefLef"
 ?lefFileName "daisyFileName"
 ?outputDir "directory")


;; (daisyCopyCells cvList "work_jawi" "Test")


(foreach cv cvList
	 (daisyDefOut
	  (dbOpenCellViewByType 
	   "work_jacobw_defLef" (cadr cv) "layoutDefLef" "maskLayout" "r") "./bottomUp/def/"))
