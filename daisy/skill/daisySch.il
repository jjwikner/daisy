
(daisyLog " ... loading %s.il ...\n" "schMods")

(defun daisySchAnnotatePin ()			  
  (let ((l_pinPerhaps (list))
	(bBox (list (list 0 0) (list 0 0)))
	(pinName "")
	(lblPos (list 0 0))
	(lblId nil)
	(pinDir "input")
	(lblJust "centerRight"))	    
    ;;    (printf "hello world\n")
    (foreach terminal  (if (geGetSelectedSet) (geGetSelectedSet) 
			 (hiGetCurrentWindow)->cellView->terminals)	     
	     (foreach pin  (if (and (equal terminal->objType "rect")
				    (equal terminal->layerName "pin"))  (list terminal)  terminal->pins)
		      (if (and (equal terminal->objType "rect")
			       (equal terminal->layerName "pin"))  
			  (setq bBox    pin -> bBox) 
			(setq bBox    pin -> fig -> bBox))
		      (setq pinName pin -> net -> name)
		      (setq pinDir  pin -> net -> term -> direction)
		      
		      (if (equal pinDir "input") then	  
			(setq lblJust "centerRight")
			(setq lblPos (list 
				      (min (caar bBox) (caadr bBox))
				      -0.0625*0+(min (cadar bBox) (cadadr bBox)))))
		      (if (equal pinDir "output") then
			(setq lblJust "centerLeft")
			(setq lblPos (list 
				      (max (caar bBox) (caadr bBox))
				      -0.0625*0+(max (cadar bBox) (cadadr bBox)))))
		      (if (equal pinDir "inputOutput") then
			(setq lblJust "centerCenter")
			(setq lblPos (list 
				      (plus (caar bBox) (caadr bBox))/2 
				      -0.0625*0+(plus (cadar bBox) (cadadr bBox))/2)))
		      (daisyGridObjectXY "xy" 1 
				       (list 
					(list 
					 (setq sLabel (schCreateSymbolLabel
						       (hiGetCurrentWindow)->cellView      
						       lblPos "analog pin annotate"     
						       (sprintf nil "cdsTerm(\"%s\")" pinName)
						       lblJust "R0" "stick"
						       0.03125 "ILLabel")))))
		      sLabel~>parent = 		      
		      (if (and (equal terminal->objType "rect")
			       (equal terminal->layerName "pin"))  
			  pin pin->fig)))))

(defun schAnnotatePin_obsolote ()			  
  (let ((l_pinPerhaps (geGetSelectedSet))
	(bBox (list (list 0 0) (list 0 0)))
	(pinName "")
	(lblPos (list 0 0))
	(lblId nil)
	(pinDir "input")
	(lblJust "centerRight"))	    
    
    (foreach pinPerhaps l_pinPerhaps
	     (if (and (equal pinPerhaps->objType "rect")
		      (equal pinPerhaps->layerName "pin")) then
	       (setq bBox    (pinPerhaps -> bBox))
	       (setq pinName (pinPerhaps -> net -> name))
	       (setq pinDir  (pinPerhaps -> net -> term -> direction))
	       
	       (if (equal pinDir "input") then	  
		 (setq lblJust "centerRight")
		 (setq lblPos (list 
			       (min (caar bBox) (caadr bBox))
			       -0.0625+(min (cadar bBox) (cadadr bBox)))))
	       (if (equal pinDir "output") then
		 (setq lblJust "centerLeft")
		 (setq lblPos (list 
			       (max (caar bBox) (caadr bBox))
			       -0.0625+(max (cadar bBox) (cadadr bBox)))))
	       (if (equal pinDir "inputOutput") then
		 (setq lblJust "centerCenter")
		 (setq lblPos (list 
			       (plus (caar bBox) (caadr bBox))/2 
			       -0.0625+(plus (cadar bBox) (cadadr bBox))/2)))
	       (daisyGridObjectXY "xy" 1 
				(list 
				 (list 
				  (schCreateSymbolLabel
				   (hiGetCurrentWindow)->cellView      
				   lblPos "analog pin annotate"                  
				   (sprintf nil "cdsTerm(\"%s\")" pinName)
				   lblJust "R0" "stick"
				   0.03125 "ILLabel"))))))))

(defun daisySchReportProbes (@optional (cellView nil) 
				     (cellNameX "daisyProbePoint") 
				     (beenThere nil))
  (let (( cw (hiGetCurrentWindow))
	(cv nil)
	(selSet nil)
	(probesList nil)
	)
    (if (not cellView) 
	(setq cv (getq cw "cellView"))
      (setq cv cellView))
    ;; Add a warning check here. (For example get a lib Id and perform dbAllCellViews and member.)
    (setq cv (dbOpenCellViewByType (getq cv "libName")  (getq cv "cellName") "schematic") )
    (setq selSet  (getq cv "instances"))
    (if selSet
	(foreach instance selSet 
		 (if (equal (getq instance "cellName") cellNameX)
		     (progn
		       ;; (printf "Probe point connected to %s in cell %s \n" 
		       ;; (getq  (getq (car (getq instance "instTerms")) "net") "name")
		       ;; (getq cv "cellName"))
		       (setq probesList (append probesList 
						(list (list (getq (getq (car (getq instance "instTerms")) "net") "name")
			    (getq cv "cellName")))))))
		 ;; Here a check could be added and check if cell already in the beenThere list
		 (setq probesList 
		       (append probesList  (daisySchReportProbes (getq instance "master"))))))
    (if probesList (printf "%L\n" probesList))
    probesList))

(defun daisyTranslateVector (pos rot)
  (let ((newPos pos)
	(xPos (car pos))
	(yPos (cadr pos)))
    (case rot
	  ("R0"      (setq newPos (list  xPos  yPos)))
	  ("R90"     (setq newPos (list -yPos  xPos)))
	  ("R180"    (setq newPos (list -xPos -yPos)))
	  ("R270"    (setq newPos (list  yPos -xPos)))
	  ("MX"      (setq newPos (list  xPos -yPos)))
	  ("MY"      (setq newPos (list -xPos  yPos)))
	  ("MXR90"   (setq newPos (list  yPos xPos)))
	  ("MYR90"   (setq newPos (list -yPos -xPos)))
	  )    
    newPos))

(defun daisyGetTerm (termName @optional (objId nil))
  (if (not objId) (setq objId (car (geGetSelSet))))  
  (car (setof validTerms objId->master->terminals (equal validTerms->name termName))))

(defun daisyGetTermPos (instTerm 
		     rot 
		     @key (bBox (list (list 0.0 0.0) (list 1.0 0.0)))
		     (pos (list 0.0 0.0))
		     (objId nil))
  ;; Get the coordinates of the terminals in a schematic figure
  
  (let ((yOffset 0) (xOffset 0) (xyTran (list 0 0)))
    ;; (printf "%L" (getq instTerm pins))

    (setq yOffset  
	  0.5*((cadr (car (getq (getq (car (getq instTerm pins)) fig) bBox)))+
 	       (cadr (cadr (getq (getq (car (getq instTerm pins)) fig) bBox)))))
    
    (setq xOffset  
	  0.5*((car (car (getq (getq (car (getq instTerm pins)) fig) bBox)))+
	       (car (cadr (getq (getq (car (getq instTerm pins)) fig) bBox))))) 

    (setq xyTran (daisyTranslateVector (list xOffset yOffset) rot))
    (printf "--> %L" xyTran)
    (list (car pos)+(car xyTran)
	  (cadr pos)+(cadr xyTran))
))

(defun textToAlist (textString)
  (let ((localAlist nil)
	(textList nil))
    (setq textList (parseString textString "\n ;,"))
    (while textList
      
      (setq localAlist (append localAlist 
			       (list (list (car textList) 
					   (if (cadr textList) (cadr textList) "vNOTDEFINED")))))
      (setq textList (cddr textList)))
    localAlist))

(defun daisyAddSupplyNames  
  (@optional 
   (supplyAlist (list (list "vpwr" "vpwr") 
		      (list "vpwrGuard" "vpwrGuard") 
		      (list "vgndGuard" "vgndGuard") 
		      (list "vgnd" "vgnd")
		      (list "vpwr33" "vpwr33")
		      (list "vpwr33dig" "vpwr33dig")
		      (list "vpwr33ana" "vpwr33ana")
		      (list "vpwr33Guard" "vpwr33Guard")
		      (list "vpwr10" "vpwr10")
		      (list "vpwr10ana" "vpwr10ana")
		      (list "vpwr10dig" "vpwr10dig")
		      (list "vpwr10Guard" "vpwr10Guard")
		      (list "vsub" "vgndGuard"))
		)
   (setOfInstances nil)
   (currentCell (hiGetCurrentWindow)->cellView ))

  (info "daisyAddSupplyNames\n")

  (let ((localXY (list 0 0))
	(labelId nil)
	(textBox nil)
	(dummySet nil) )
    
    (if (not setOfInstances) 
	(setq setOfInstances currentCell->instances)
      )
    (setq dummySet    (geGetSelectedSet))
    
    (if dummySet
	(if (equal (car dummySet)->objType "label") 
	    (progn 
	      (setq supplyAlist (textToAlist (car dummySet)->theLabel))
	      (if (greaterp (length dummySet) 1) (setq setOfInstances (cdr dummySet))))
	  (setq setOfInstances dummySet)))
    (printf "dummyset: %L \n" dummySet)
   
    (foreach inst setOfInstances
	     (setq localXY inst->xy)
	     (printf "%L\n" localXY) 
	     (printf "hehhe -- \n")
	     (foreach term inst~>master~>terminals ;; sigNames ;; instTerms
		      
		     (if (assoc term->name supplyAlist)
			  (progn 
			    (printf "%L " currentCell)
			    (printf "term %s \n" term->name)
			   
			    ;; Add a removal of old labels ?
			    term->term
			    (daisyGetTermPos term inst->orient )
			    (printf "hehhe ==\n")
			    (setq labelId 
				  (dbCreateLabel 
				   currentCell (list "wire" "label")
				   (list (plus (car localXY) (car (daisyGetTermPos term inst->orient )))
					 (plus (cadr localXY) (cadr (daisyGetTermPos term inst->orient))))
				   (strcat (if 
					       (greaterp inst->numInst 1)
					       (sprintf nil "<*%d>" inst->numInst)
					     "")
					   (cadr (assoc term->name supplyAlist)))
				   "centerCenter" "R0" "stick" 0.0625))
			    labelId~>parent = inst))))))
  



(defun daisyDeCypress (@optional (cv nil))
  (let ((propsToRemove (list "powerSignals" "mosFetFoldWidth" 
			     "interfaceLastChanged" "icecapNode" 
			     "iceChoice" "iceCapScale" 
			     "groundSignals" "diodeChoice" 
			     "diodeACM" "SCHlastSchematicExtraction")))
    
    (if (not cv) (setq cv (hiGetCurrentWindow)->cellView))
    (foreach cyProp propsToRemove 
	     (printf "%30s : %12s removed! \n" cyProp (if (dbGetPropByName cv cyProp)->name " being " " already " ))
	     (dbDeletePropByName cv cyProp)	 )
    (dbSetq cv cv->cellName  "partName")))

(defun daisyDeCypressAll (@optional (libId nil))
  ;; Parse through a given library and removed the properties.
  (foreach cv (getCellViews libId)
	   (dbOpenCellViewByType cv "symbol" "a")
	   (daisyDeCypress cv)
	   (dbSave cv)))

(defun daisyShowDate ( cellView )
  (if (isCallable 'dbGetDatabaseType) 
      (if (equal (dbGetDatabaseType) "CDBA")
	  cellView~>instancesLastChanged 
	cellView~>fileTimeStamp) 
    cellView~>instancesLastChanged))

(defun daisySchOwner ( cellView )
       (getShellEnvVar "USER"))
  
(defun daisyInheritSymbol (@optional (libName "dummyLib") )
  (let ((symbolView nil) (schemaView nil) 
	(symTermList (list )) (currentLib nil) (hitMe nil))
    
    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))
    
    (foreach cell currentLib->cells

	     (setq symTermList nil)
	     
	     (setq symbolView (dbOpenCellViewByType cell->lib->name cell->name "symbol" nil "r"))
	     (setq schemaView (dbOpenCellViewByType cell->lib->name cell->name "schematic" nil "a" ))
	     
	     (if (and symbolView schemaView) 
		 (progn 
		   (foreach terminal symbolView->terminals
			    ;; (printf "%L \n" (list terminal->name terminal->direction  ))
			    (setq symTermList (append (list (list terminal->name terminal->direction)) symTermList)))
		   (foreach terminal schemaView->terminals
			    ;; (printf "%L \n" (list terminal->?? terminal->direction  ))
			    (setq hitMe (cadr (assoc terminal->name symTermList)))
			    (if (and hitMe  (not (equal hitMe terminal->direction) ))
				(progn
				  (printf "Port mismatch on port %3s. Direction is changed from %11s to %11s. \n" 
					  terminal->name terminal->direction hitMe)
				  (terminal->direction = hitMe))))
		   (dbCheck schemaView)
		   (dbSave schemaView)
		   (dbClose schemaView)
		   ))))
  t)

(defun daisyFindPortMismatch (@optional (libName "dummyLib") (fixProblem nil))
  (let ((symbolView nil) (schemaView nil)  (minX 0) (minY 0)
	(symTermList (list )) (schTermList (list )) (portsToAdd (list))
	(currentLib nil) (hitMe nil))
    
    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))
    
    (foreach cell currentLib->cells
	     
	     (setq symTermList nil)
	     (setq schTermList nil)
	     (setq portsToAdd nil)
	     (setq symbolView (dbOpenCellViewByType cell->lib->name cell->name "symbol" nil "r"))
	     (setq schemaView (dbOpenCellViewByType cell->lib->name cell->name "schematic" nil 	     (if fixProblem "a" "r")))
	     (setq ipinView (dbOpenCellViewByType "basic" "ipin" "symbol" nil "r"))
	     (setq opinView (dbOpenCellViewByType "basic" "opin" "symbol" nil "r"))
	     (if (and symbolView schemaView) 
		 (progn 
		   
		   (if (not (equal (length symbolView->terminals) (length schemaView->terminals)))
		       (progn
			 (foreach terminal symbolView->terminals
				  (setq symTermList (append (list terminal->name) symTermList)))
			 (foreach terminal schemaView->terminals
				  (setq schTermList (append (list terminal->name) schTermList)))
			 (setq symTermList (sort symTermList nil))
			 (setq schTermList (sort schTermList nil))			 
			 (foreach symTerm symTermList
				  (if (not (member symTerm schTermList))
				      (setq portsToAdd (append (list symTerm) portsToAdd))))
			 (printf "Port mismatch in %s : %L vs %L. Ports to add: %L \n " cell->name symTermList schTermList portsToAdd)
			 (if fixProblem (daisyAddPins schemaView portsToAdd))))))) )
  t)

(defun daisyReportCell (@optional (cellView nil))
  (let ((tempCellInfo "")
	(cellList nil))
    
    (if (not cellView) 
	(setq cellView (geGetSelectedSet))
      (if (not (listp cellView))
	  (setq cellView (list cellView))))
    
    (foreach partCell cellView
	     (sprintf tempCellInfo
		      "_%s_%s_"
		      partCell->libName
		      partCell->cellName)
	     (setq cellList (append (list tempCellInfo) cellList)))
    cellList))
  
(defun daisyReportPorts (@optional (cellView nil))
  (let ((csvFileName "") (csvPort nil) 
	(portList nil) (stringOfFiles "")
	(powerList (list "vpwr10" "vpwr" "vpwr33" "vpwr33Guard" "vpwrGuard" "vpwr10Guard" "vdd" "VDD"))
	(groundList (list "vgnd" "vgndGuard" "vss" "VSS"))    )

    (if (not cellView) 
	(setq cellView (geGetSelectedSet))
      (if (not (listp cellView)) (setq cellView (list cellView))))
    
    (foreach partCell cellView
	     (sprintf csvFileName 
		      "%s/log/%sportInfo"
		      (getShellEnvVar "USERAREA") 
		      (car (daisyReportCell partCell)))
	     (if (isFile (strcat csvFileName ".csv"))
		 (setq csvFileName (strcat (makeTempFileName csvFileName) ".csv"))
	       (setq csvFileName (strcat csvFileName ".csv")))    
	     (setq csvPort (outfile csvFileName))
	     (setq stringOfFiles (strcat stringOfFiles " " csvFileName))
	     (if (boundp 'csvPort)
		 (if csvPort
		     (fprintf csvPort (csvList (list "Port Name" "Direction" "Power Domain" "Levels" "Activity" "Description")))))
	     
	     (if (boundp 'csvPort)
		 (if csvPort
		     (foreach term partCell->instTerms
			      (fprintf csvPort 
				       (csvList (list term->name 						      
						      (if (member term->name powerList) "power" 
							(if (member term->name groundList) "ground" 
							  term->direction))))))))
	     
	     (if (boundp 'csvPort) (if csvPort (progn 
						 (close csvPort)))))
    (if (greaterp (strlen stringOfFiles) 2)
	(sh (sprintf nil "openoffice.org %s &" stringOfFiles)))))

(defun daisyUpdateTransWidth (@optional (libName "test3") (refLib "tsmcN40") (viewName "schematicA"))
  (let (( currentLib nil))
    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))    
    (foreach cell  currentLib->cells
	     (setq schemaView 
		   (dbOpenCellViewByType 
		    cell->lib->name 
		    cell->name 
		    viewName 
		    nil "a" )
		   )
	     (if schemaView 
		 (progn
		   (foreach inst (setof x schemaView->instances (equal x->master->libName refLib))
			    inst->Wfg = inst->w
			    )
		   (dbCheck schemaView)
		   (dbSave schemaView)
		   (dbClose schemaView)
		   )
	       )
	     )
    )
  )

(defun daisyChangeToHvt (@optional (libName "tcbn40lpbwphvt_110a") (refLib "tsmcN40") (viewName "schematic"))
  (let (( currentLib nil) (m40pch_hvt nil) (m40nch_hvt nil)  (transAssocList nil) )

    (setq m40pch_hvt   (dbOpenCellViewByType refLib "pch_hvt"    "symbol"))
    (setq m40nch_hvt   (dbOpenCellViewByType refLib "nch_hvt"    "symbol"))
    (setq m40pch_33    (dbOpenCellViewByType "tsmcN40" "pch_25"     "symbol"))
    (setq m40nch_33    (dbOpenCellViewByType "tsmcN40" "nch_25"     "symbol"))

    
    (setq 
     transAssocList 
     (list 
      (list "pch"      m40pch_hvt)
      (list "nch"      m40nch_hvt)
      (list "pch_25"      m40pch_33)
      (list "nch_25"      m40nch_33)
      
      (list "pch_hvt"      m40pch_hvt)
      (list "nch_hvt"      m40nch_hvt)
      ))

    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))    
    (foreach cell  currentLib->cells
	     (setq schemaView 
		   (dbOpenCellViewByType 
		    cell->lib->name 
		    cell->name 
		    viewName 
		    nil "a" )
		   )
	     (if schemaView 
		 (progn
		   (printf "%s\n " cell->name)
		   (foreach transId  
			    (setof x schemaView->instances 
				   (and 
				    (equal x->libName refLib)
				    (equal (substring x->cellName 2 2) "ch")))
			    
			    transId->master=(cadr (assoc transId->cellName transAssocList))
			    )
		   
		   (dbCheck schemaView)
		   (dbSave schemaView)
		   (dbClose schemaView)
		   )
	       )
	     )
    )
  )

(defun daisyCopyCells (@optional 
		       (libNameFrom "tcbn40lpbwp_110a")
		       (libNameTo   "tcbn40lpbwphvt_110a")
		       (viewNameFrom "symbol") 
		       (viewNameTo "symbol")
		       (tag "HVT")
		       )
  (let (( currentLib nil) (newCell nil))
    (foreach lib (ddGetLibList)
	     (if (equal libNameFrom lib->name) 
		 (setq currentLib lib)))    
    (foreach cell  currentLib->cells
	     (setq schemaView 
		   (dbOpenCellViewByType 
		    libNameFrom
		    cell->name 
		    viewNameFrom
		    nil "r" )
		   )
	     (if schemaView 
		 (progn
		   (setq newCell 
			 (dbCopyCellView 
			  schemaView 
			  libNameTo
			  (strcat cell->name tag)
			  viewNameTo 
			  "" nil t))
		   printf("%L\n " newCell)
		   ;; (dbPurge schemaView)
		   (if newCell 
		       (progn
			 (printf "%s\n" newCell->cellName)
			 ;; (dbSave newCell)
			 ;; (dbPurge newCell)
			 )))))))

(defun daisyChangeGlobalNets (@optional (libName "test3") )
  (let ((symbolView nil) 
	(symTermList (list )) (currentLib nil) (hitMe nil)
	(vpwrPort nil) (vgndPort nil) (vddlPort nil) (aList nil))
    
    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))
    
    
    (setq vpwrPort (dbOpenCellViewByType "basic" "vdd_inherit" "symbol" "" 'r ))
    (setq vgndPort (dbOpenCellViewByType "basic" "gnd_inherit" "symbol" "" 'r ))
    (setq vddlPort (dbOpenCellViewByType "basic" "vcc_inherit" "symbol" "" 'r ))
    (setq aList (list 
		 (list "gnd!" vgndPort "_vss") 
		 (list "VDD!" vpwrPort "_vpwr") 
		 (list "VDDL!" vddlPort "_vddl")
		 (list "VSS!" vgndPort "_vss") 
		 (list "vdd!" vpwrPort "_vpwr") 
		 (list "VDDL!" vddlPort "_vddl")
		 ))

    (foreach cell  currentLib->cells
	     
	     ;; (setq symbolView (dbOpenCellViewByType cell->lib->name cell->name "symbol" nil "r"))
	     (setq symbolView t)
	     (setq schemaView (dbOpenCellViewByType cell->lib->name cell->name "schematicA" nil "a" ))
	     (setq symTermList nil)
	     
	     (if (and symbolView schemaView) 
		 (progn  
		   (foreach element schemaView->shapes
			    (if (equal element->objType "label")
				(if (assoc element->theLabel aList)
				    (dbDeleteObject element))))
;;		   (foreach terminal symbolView->terminals
			    (setq deleteItQuestion nil)
			    (foreach terminal schemaView->terminals
				     (if (assoc terminal->name aList)
					 (progn
					   (printf "%s : %s Origin: %L \n" 
						   terminal->name
						   cell->name 
						   (car terminal->pins)->fig->xy)
					   (dbCreateInst schemaView
							 (cadr (assoc terminal->name aList))
							 (strcat "I" cell->name (caddr (assoc terminal->name aList)))
							 (car terminal->pins)->fig->xy "R0")
					   (setq deleteItQuestion t)))			     
				     (if deleteItQuestion 
					 (progn 
					   (dbDeleteObject (car terminal->pins)->fig)
					   (dbDeleteObject terminal))))
					;)
			    (dbCheck schemaView)
		   (dbSave schemaView)
		   (dbClose schemaView)))))
  t)

(defun daisyChangeGlobalNetsTwo (@optional (libName "tsmcN80Pads_aio") )
  (let (	(symTermList (list )) (currentLib nil) (hitMe nil) (deleteItQuestion nil)
		(vpwrPort nil) (vgndPort nil) (vddlPort nil) (aList nil))
    
    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))
    
    (setq vpwrPort (dbOpenCellViewByType "basic" "vdd_inherit" "symbol" "" 'r ))
    (setq vgndPort (dbOpenCellViewByType "basic" "gnd_inherit" "symbol" "" 'r ))
    (setq vddlPort (dbOpenCellViewByType "basic" "vcc_inherit" "symbol" "" 'r ))
    (setq vssPort  (dbOpenCellViewByType "basic" "vss_inherit" "symbol" "" 'r ))
    (setq aList (list (list "VSS!" vgndPort "_vss") 
		      (list "TACVDD" vpwrPort "_vpwr") 
		      (list "xTxxACVDD" vssPort "_vcc") 
		      (list "TAVDD33" vddlPort "_vddl")))
    
    (foreach cell  currentLib->cells
	     (setq schemaView (dbOpenCellViewByType cell->lib->name cell->name "schematic" nil "a" ))
	     (setq deleteItQuestion nil)
	     (if schemaView
		 (progn  
		   (printf "%s \n" schemaView->cell->name)
		   (foreach element schemaView->shapes
			    (if (equal element->objType "label")
				(progn
				  (printf "%s \n" element->theLabel)
				  (if (assoc element->theLabel aList)
				      (dbDeleteObject element)))))
		   (foreach terminal schemaView->terminals
			    (if (assoc terminal->name aList)
				(progn
				  (printf "%s : %s Origin: %L \n" 
					  terminal->name
					  cell->name 
					  (car terminal->pins)->fig->xy)
				  (dbCreateInst schemaView
						(cadr (assoc terminal->name aList))
						(strcat "I" cell->name (caddr (assoc terminal->name aList)))
						(car terminal->pins)->fig->xy "R0")
				  (setq deleteItQuestion t)))
			    (if deleteItQuestion 
				(progn 
				  (dbDeleteObject (car terminal->pins)->fig)
				  (dbDeleteObject terminal))))
		   (dbCheck schemaView)
		   (dbSave schemaView)
		   (dbClose schemaView)))))
  t)

(defun daisyCheckLock (@optional (cellView nil))
  (let (( fileName "")) 
    (if (not cellView) 
	(setq cellView (getq (hiGetCurrentWindow) "cellView")))
    (setq fileName 
	  (strcat (cellView->fileName) ".cdslck"))
    
    (if (isFile fileName)
	(progn 
	  (sh (sprintf nil  "%s/bin/daisyReportCdsUser.sh %s" 
		       (getShellEnvVar "DAISYAREA") 
		       fileName))
	  (loadi (strcat "/tmp/" "daisyLockUser" ".il"))
	  (sprintf nil "%s" LOCKUSER  ))
      "")))

(defun daisyAddPins (cellView pinsAdded)
  (let ((termNames nil) (terminals nil) (xLoc 0) (yLoc 0))
    (setq terminals cellView~>terminals)
    (setq termNames terminals~>name)
    (foreach pinToAdd pinsAdded
	     ;; If this terminal doesn't exist
	     (unless (member pinToAdd termNames)
	       
	       (setq snapGrid (envGetVal "schematic" "schSnapSpacing"))	       
	       ;; get the pin creation location, and make sure that
	       ;; they land on grid
	       (setq xLoc (plus (caar cellView~>bBox) 1.0))
	       (setq yLoc (difference (cadar cellView~>bBox) 0.2))
	       (foreach x '(xLoc yLoc)
			(set x (times snapGrid (round (quotient (eval x)
								snapGrid)))))
	       ;; instances are created differently in 4.3.4 & 4.4
	       (cond ((isCallable 'dbCreateInstByMasterName)
		      (setq thisPin (dbCreateInstByMasterName cellView "basic" "ipin" "symbol" pinToAdd (list xLoc yLoc) "R0")))
		     
		     ((isCallable 'dbCreateInst)
		      (setq ipin (dbOpenCellView (dbOpenLib "basic" "" "" "r") "ipin" "symbol"))
		      (setq thisPin (dbCreateInst cellView ipin pinToAdd xLoc:yLoc "R0")))
		     (t (setq thisPin nil)))

	       thisPin~>purpose = "pin"
	       (setq thisnet (dbMakeNet cellView pinToAdd))
	       (dbCreateTerm thisnet pinToAdd "input")
	       (dbCreatePin thisnet thisPin pinToAdd)
	       
	       ;; update the "portOrder" property if it exists
	       (when (and cellView~>portOrder pinsAdded)
		 (unless (member pinToAdd cellView~>portOrder)
		   (dbReplaceProp cellView "portOrder" "list"
				  (append cellView~>portOrder (list pinToAdd)))))))
    ;; if this schematic has a port order property, make sure that it
    ;; does not deviate from the "golden" port order property
    (when cellView~>portOrder
      (dbReplaceProp cellView "portOrder" "list"
		     (PPPfindGoldenPortOrder cellView~>libName
					     cellView~>cellName
					     cellView~>terminals~>name
					     cellView~>portOrder)))))

(defun daisyStripDaisy (string @optional (projectTag (getShellEnvVar "PROJNAME")))
  (rexCompile  projectTag)
  (setq string (rexReplace string "" 1))

  (rexCompile "daisy")
  (setq string (rexReplace string "" 1))

  )


(defun daisyAddSymbolName (cellView @optional (logicalLabel t))
  (let ((centerX 0) (centerY 0) (heigth 0) (labelId nil))
    (setq centerX (caar cellView->bBox)+ 0.5*((caadr cellView->bBox)-(caar cellView->bBox)))
    (setq centerY (cadar cellView->bBox)+0.5*((cadadr cellView->bBox)-(cadar cellView->bBox)))
    (setq height  0.8*((cadadr cellView->bBox)-(cadar cellView->bBox)))

    (setq labelId (dbCreateLabel cellView 
				 (list "device" "label") 
				 (list centerX centerY+0.25*height) 
				 (if logicalLabel 
				     "(daisyStripDaisy ilInst~>cellName)||\"<cellName>\"" cellView->cellName)
				 "centerCenter" "R0" "stick" 
				 0.0625 ))
    labelId->labelType = (if logicalLabel "ILLabel" "NLPLabel")

    (setq labelId (dbCreateLabel cellView 
				 (list "device" "label") 
				 (list centerX centerY) 
				 (if logicalLabel 
				     "(daisyStripDaisy ilInst~>libName)||\"<libName>\"" cellView->libName)
				 "centerCenter" "R0" "stick" 
				 0.0625 ))
    labelId->labelType = (if logicalLabel "ILLabel" "NLPLabel")
    
    (setq labelId (dbCreateLabel cellView 
				 (list "instance" "label") 
				 (list centerX centerY-0.25*height) 
				 (if logicalLabel "[@instanceName]" cellView->cellName)
				 "centerCenter" "R0" "stick" 
				 0.0625 ))
    labelId->labelType = "NLPLabel"
    (dbSave cellView)
    ))

(defun daisyAddSymbolNames (@optional (libName  "tsmcN80_scx80gchvt"))
  (let ((symbolView nil) 
	(symTermList (list )) (currentLib nil) (hitMe nil))
    
    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))
    (foreach cell  currentLib~>cells
	     (setq symbolView (dbOpenCellViewByType cell->lib->name cell->name "symbol" nil "a"))	     
	     (if symbolView (daisyAddSymbolName symbolView)))))

(defun daisyUpdateLibCDFs (@optional (libName  "tsmcN80_scx80gc"))
  (let ((schemaView nil) 
	(symTermList (list )) (currentLib nil) (hitMe nil))
    
    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))
    (foreach cell  currentLib~>cells
	     (setq schemaView (dbOpenCellViewByType cell->lib->name cell->name "schematic" nil "a"))	     
	     (if schemaView 
		 (progn 
		   (artGenerateHierSymbolCDF schemaView t)
		   (dbSave schemaView))))))

(defun daisyReportFingerM (@optional (cellView nil))
  (if (not cellView)
      (setq cellView (hiGetCurrentWindow)->cellView))
  (printf "<=========== List of instances and their multipliers, etc.\n")
  (foreach inst cellView->instances
	   (if (equal inst->libName "tsmcN80")
	       
	       (printf "-----------> Instance %s (%s): fingers=%s and (total) m=%s. \n" 
		       inst->name 
		       inst->cellName 
		       (if inst->fingers inst->fingers "--")
		       (if inst->simM inst->simM "--")))))


(defun daisyUpdateFingerM (@optional (cellView nil))
  ;; Updates the finger properties after a faulty calibre extraction.

  (let ( ( transList (list "pch_33" "pch" "nch" "nch_33")) (newId nil)
	 ( viewList  (list "extracted" "extractedC" "extractedRC")) )
    
    (if (not cellView) 
	(setq cellView (hiGetCurrentWindow)->cellView))
    
   ;; (dbAllCellViews cellView->libName cellView->cellName)
    
    (if (member cellView->viewName viewList)
	(progn
	  (printf "<============ Changing all transistor multipliers and fingers to m = 1, finger = 1. \n")
	  (printf " ============ Copying old cellview to %s_simM        ============> \n" cellView->viewName)
	  (setq newId (dbCopyCellView cellView cellView->libName cellView->cellName (strcat cellView->viewName "_simM") nil "charlie" t))
	  (if newId 
	      (progn
		(dbCopyProp cellView newId)
		(dbSave newId)
		(dbPurge newId)))
	  (foreach inst cellView->instances
		   (if (equal inst->libName "tsmcN80")
		       (if (member inst->cellName transList)
			   (progn
			     inst->fingers = "1"
			     inst->simM = "1"
			     ;;(printf "x --- %s \n " inst->simM)
			     ))))
	  )
      (progn
	(printf "<============ THIS IS NOT CONSIDERED TO BE AN EXTRACTED VIEW =============>\n")
	))
    ) t )


(defun daisyModDiode ()
  ;; just a script to run on tsmc diodes in standard cell library
  (foreach propp (car (geGetSelSet)~>prop)
	   (if (equal propp->name "area") propp->value = "1e-13")
	   (if (or (equal propp->name "w") (equal propp->name "l"))
	       propp->value = "316n")))



(defun daisyToggleImportantNote ()
  (foreach textLabel (geGetSelSet)
	   (if (equal textLabel->objType "label")
	       (if (not (equal textLabel->layerName "wire"))
		   (if (not (dbSearchPropByName textLabel "ImportantNote"))
		       (daisyDefineImportantNote textLabel)
		     (daisyDeleteImportantNote textLabel))))))

(defun daisyDefineImportantNote (@optional (textLabel nil))
  (let (
	(textBox nil))

    (if (not textLabel)
	(setq textLabel (car (geGetSelSet))))
    
    textLabel->layerName = "hilite"
    textLabel->purpose   = "drawing1"
    textLabel->font = "roman"
    (dbCreateProp textLabel "ImportantNote" "string" 
		  (strcat "Important Note Before Tapeout:"
			  textLabel->theLabel)) 
    (dbReplaceProp textLabel "ImportantNote" "string" 
		   (strcat "Important Note Before Tapeout:"
			   textLabel->theLabel)) 
    (setq textBox 
	  (dbCreateLine textLabel->cellView 
			textLabel->lpp 
			(list (car textLabel->bBox) 
			      (list (caadr textLabel->bBox) 
				    (cadar textLabel->bBox)))))
    (dbCreateProp textBox "ImportantNote" "string" "ImportantNote")
    (leAttachFig textBox textLabel)   
    ))

(defun daisyDeleteImportantNote (@optional (textLabel nil))
  (if (not textLabel)
      (setq textLabel (car (geGetSelSet))))  
  textLabel->layerName = "text"
  textLabel->purpose   = "drawing"
  textLabel->font = "stick"
  (dbDeletePropByName textLabel "ImportantNote")
  (foreach child textLabel->children
	   (if (dbSearchPropByName child "ImportantNote")
	       (dbDeleteObject child))))


(defun daisyOpenSomeCells ()
  (let (( currentCellId (car (geGetSelSet))) )
    
    (if (not currentCellId)
	(progn
	  (setq currentCellId (hiGetCurrentWindow)->cellView)
	  (if (equal currentCellId->viewName "schematic")
	      (geOpen ?window (hiOpenWindow)
		      ?lib currentCellId->libName
		      ?cell currentCellId->cellName
		      ?view "layout"
		      ?viewType "maskLayout"
		      ?mode "r"))
	  (if (equal currentCellId->viewName "layout")
	      (geOpen ?window (hiOpenWindow)
		      ?lib currentCellId->libName
		      ?cell currentCellId->cellName
		      ?view "schematic"
		      ?viewType "schematic"
		      ?mode "r")))
      (progn
	(geOpen ?window (hiOpenWindow)
		?lib currentCellId->libName
		?cell currentCellId->cellName
		?view "schematic"
		?viewType "schematic"
		?mode "r")	
	(geOpen ?window (hiOpenWindow)
		?lib currentCellId->libName
		?cell currentCellId->cellName
		?view "layout"
		?viewType "maskLayout"
		?mode "r") ))))


(defun daisyReplaceStdCells (@optional (svtToHvt nil) (cvId nil ) )
  
  (let (( cellMaster nil ))
  (if (not cvId )
      (setq cvId (hiGetCurrentWindow)->cellView))
  rexCompile("HVT")
  (foreach inst (setof x cvId->instances 
		       (or 
			(equal x->libName "tcbn90ghp_150a") 
			(equal x->libName "tcbn90ghphvt_150a") ))
	   
	   (setq newName (rexReplace inst->cellName "" 1))
	   
	   (setq cellMaster 
		 (dbOpenCellViewByType 
		  (if (and 
		       (not svtToHvt)
		       (equal inst->libName "tcbn90ghp_150a"))
		      "tcbn40lpbwp_110a"
		    "tcbn40lpbwphvt_110a" )			 
		  (if (and 
		       (not svtToHvt)
		       (equal inst->libName "tcbn90ghp_150a"))
		      (strcat newName "BWP")
		    (strcat newName "BWPHVT")
		    )
		  "symbol") )
	   (if cellMaster 
	       (progn
		 inst->master = cellMaster
		 (dbClose cellMaster)))	   
	   )
  
  (if cvId 
      (progn
	;; (dbCheck cvId)
	;;(dbSave cvId)
	;; (dbClose cvId)
	(printf "jjeje")
	))
  )
)

(defun daisyToggleAnnotate (@optional (status nil) )
  (let ( ( techId (techGetTechFile (hiGetCurrentWindow)->cellView))
	 (layerPairList (list (list "annotate" "drawing")
			      (list "pin" "drawing")
			      (list "annotate" "drawing7")
			      (list "annotate" "drawing8")
			      )))
    ;; (leIsLayerValid (list "annotate" "drawing") techId)
    (foreach layerPair layerPairList
	     (leSetLayerValid layerPair t techId)
	     
	     (if (and (leIsLayerVisible layerPair techId)
		      (not status))
		 (leSetLayerVisible  layerPair nil techId)
	       (leSetLayerVisible  layerPair t techId)))	       
    )
  )


(defun daisyOpenHyperLink (object)
  (let ( (labelString "") )
    (if (equal (object->objType) "label")
	(progn
	  (setq labelString object->theLabel)
	  (printf " ~~~~~> Opening %s \n" labelString)
	  (sh (sprintf nil "firefox %s &" labelString))))))

(defun daisyDisplayInfoFile (@optional (cvId nil))
  (let (
	(filePath "")
	(filePointer nil)
	(readLine "")
	) 
    (if (not cvId) (setq cvId (car (geGetSelSet))->master->cellView))
    (if (ddGetObj cvId->libName cvId->cellName "info")
	(progn
	  (setq filePath 
		(strcat 
		 (ddGetObj cvId->libName cvId->cellName "info")->readPath
		 "/text.txt"))
	  (printf "Cell information for %s / %s \n" 
		  cvId->libName cvId->cellName)
	  (printf "=========================================================\n")
	  (setq filePointer (infile filePath))
	  ;; (fscanf filePointer "%s\n" readLine)
	  (gets readLine filePointer)
	  (while readLine
	    (printf "%s" readLine)
	    (gets readLine filePointer)

	    )
	  
	  (close filePointer)
	  (printf "\n=========================================================\n")
	  ;; (printf "%s \n" filePath)
	  )
      (printf "No information available for %s %s \n" 
		  cvId->libName cvId->cellName)

      ))
  )

;; (daisyDisplayInfoFile)


    
(defun daisyCreateInfoDisplayButton (libName)

;; This function parses all cells of a library and 
;; assigns a new CDF parameter to them.

(let (( ddLibId  (ddGetObj libName))
      (cdfId nil ))
  
  (foreach cell ddLibId->cells
	   (setq cdfId  (cdfGetBaseCellCDF cell))
	   (cdfCreateParam cdfId 
			   ?name "displayInfo"
			   ?type "button"
			   ?units nil
			   ?parseAsNumber nil
			   ?prompt "Display cell info"
			   ?choices nil
			   ?use "t"
			   ?display "t"
			   ?editable nil
			   ?callback "(daisyDisplayInfoFile)")
	   (cdfSaveCDF cdfId))
  ))





(defun daisyGetCellComments (@optional (cv nil) )
  
  (if (not cv)
      (setq cv (hiGetCurrentWindow)->cellView)
    )
  (if cv->comments      
      cv->comments
    (if cv->Comments
	cv->Comments
      ""))
)

(defun daisyLike (really)
  (if really 
      (car (geGetSelSet))->Like = (car (geGetSelSet))->Like + 1
      (car (geGetSelSet))->Like = (car (geGetSelSet))->Like - 1)
  (if (car (geGetSelSet))->Like < 0 
      (car (geGetSelSet))->Like = 0 )
      
  )
  
