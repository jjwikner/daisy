(printf " ... loading %s.il ...\n" "cyUseful")


(defun daisyFlatHierarchy (hierTreeList)
;;  A = (TREEsch t nil)
  (let ( (flatList nil) ) 

    (if (listp hierTreeList)
	(foreach treeElement hierTreeList
		 schSet = (car (setof x (car treeElement)~>views (equal x->name "schematic")))
		 (if schSet
		     (setq flatList (append flatList (list schSet->cell))))
		 )
      (printf "===> %L \n" hierTreeList)
      )
    
  flatList)
)


(defun listDiff (oneList refList)
  (let ((newList nil))
    (foreach refElement refList
	     (if (not (member refElement oneList))
		 (setq newList (append newList (list refElement)))))
    newList))

(defun printProps (cellview)
  (foreach prop (cellview->master)->prop
	   (printf "=== %s  \n" prop->name)))

(defun LibTraverse (function libName @key (viewList nil) (cellList nil) (mode "r"))
  (let (cell)    
    (foreach cell (ddGetObj libName)~>cells
	     (if (and (or (null cellList) (member cell~>name cellList))) then 
	       (foreach view (dbAllCellViews (ddGetObj libName) cell~>name)
			(if (and (or (null viewList) 
				     (member view viewList))) then
			  (if (setq cv (dbOpenCellViewByType libName 
							     cell~>name 
							     view nil mode))
			      (apply function (list cv))
			    (printf "ERROR: couldn't open %s/%s/%s to run function\n" 
				    libName cell~>name view)))))) t))

(defun LISTsortUnique (lst @optional (scmp 'alphalessp) (ecmp 'equal))
  (let (next)
    (setq next (setq lst (sort lst scmp)))
    (while (cdr next)
      (if (funcall ecmp (cadr next) (car next)) then
	(rplacd next (cddr next))
	else
	(setq next (cdr next)) ))
    lst ))

(defun LISTcompare (lst1 lst2 compareFunc "llu")
  (let ((l1nil2 (tconc nil t))
	(l2nil1 (tconc nil t))
	(common (tconc nil t))
	compareVal )
    (while (or lst1 lst2)
      (cond ((null lst1)
	     (foreach elt lst2
		      (tconc l2nil1 elt) )
	     (setq lst2 nil))
	    ((null lst2)
	     (foreach elt lst1
		      (tconc l1nil2 elt) )
	     (setq lst1 nil))
	    ((zerop
	      (setq compareVal
		    (funcall compareFunc (car lst1) (car lst2)) ))
	     (tconc common (car lst1))
	     (setq lst1 (cdr lst1))
	     (setq lst2 (cdr lst2)) )
	    ((plusp compareVal)
	     (tconc l2nil1 (car lst2))
	     (setq lst2 (cdr lst2)) )
	    ((minusp compareVal)
	     (tconc l1nil2 (car lst1))
	     (setq lst1 (cdr lst1)) )))
    (list (cdar l1nil2) (cdar l2nil1) (cdar common)) ))

(defun LISTsubset (lst first last "lxx")
  (let ((count 0)
	(sublist (tconc nil t)) )
    (foreach elt lst
	     (and (geqp count first)
		  (leqp count last)
		  (tconc sublist elt) )
	     count++ )
    (cdar sublist) ))

;; License manager ================================================

(setq LMdefaultTimeout 120)

(setq LMlicenseMap
      '(
	("Symbol"                      ("34500" "95100"))
	("Schematics"                  ("95100" )) ;; 34500
	("analogArtist-Schematic"      ("34500" "34510" "95100" "95200"))
	("Layout"                      ("300" "311" "95320" "95310" "95300"))
	("Virtuoso XL"                 ("300" "3000" "95320"))
	("Other-MaskLayout"            ("300" "311" "95320" "95310" "95300" ))
	("Abstract"                    ("300")) ))

(setq LMlicenseList
;;      (list "300" "34500" "34510" "95100" "95200" "95220" "95300" "95310" "95320"))
      (list "300" "34510" "95100" "95200" "95220" "95300" "95310" "95320"))

(setq LMinUse nil)

(defun LMsetupTriggers ()
  (when (isCallable 'deRegUserTriggers)
    (foreach view (deGetAllRootTypes)
	     (deRegUserTriggers view 'LMlicenseUserAppTrigger))
    (LMstartTimers)
    t))

(defun LMlicenseUserAppTrigger (args)
  (when (equal args->action 'close)
    (LMcheckInUnused args))
  t)

(defun LMcheckInUnused (args)
  (let (application inUse)    
    ;; Determine list of applications/licenses currently being used
    (foreach window (hiGetWindowList)
	     (when (or (nequal window args->window) 
		       (nequal (deGetEditLevel args->window) 0))
	       (setq application (hiGetAppType window))
	       (printf "%s\n" application)
	       (setq inUse 
		     (append inUse (cadr (assoc application LMlicenseMap))))))
    
    ;; Checkin unused licenses
    (setq LMinUse inUse)
    (lmFeatList 'LMcheckIn)))

(defun LMcheckIn (featureList)
  (cond 
   ((null   featureList) 
    nil )
   ((stringp featureList)
    (unless (member featureList LMinUse)      
      (when (lmCheckIn featureList)
	(printf "Checking in license %s\n" featureList)
	(list featureList)
	)))
   (t
    (nconc (LMcheckIn (car featureList))
	   (LMcheckIn (cadr featureList)) ))))

(defun LMwakeup (child data)
  (let (application inUse hasChanged instancesLastChanged 
		    timeout currentTime cellView)
    (setq timeout LMdefaultTimeout)
    (setq currentTime (getCurrentTime))    
    ;; Determine list of applications/licenses currently being used

    (foreach window (hiGetWindowList)
	     (when (deGetCellView window)
	       (setq hasChanged nil)
	       (for level 0 (deGetMaxEditLevel window)
		    (setq cellView (deGetCellView window level))
		    (when (dbobjectp cellView)
		      (setq instancesLastChanged 
			    cellView~>instancesLastChanged)
		      (when (and 
			     (stringp instancesLastChanged)
			     (lessp
			      (compareTime currentTime instancesLastChanged)
			      timeout ))
			(setq hasChanged t) )))
	       (when hasChanged
		 (setq application (hiGetAppType window))
		 (setq inUse 
		       (append inUse 
			       (cadr (assoc application LMlicenseMap))) ))))
    ;; Checkin unused licenses
;;    (printf "Knock! Knock! %L \n" inUse)
;;    (printf "was ist den dass: %L \n" (list application))
   
    (setq LMinUse inUse)

    ;; (printf "%L : \n %L\n" LMinUse (assoc application LMlicenseMap)) 
    
;;    (printf "aha %L : \n" (list (listDiff inUse LMlicenseList)))
    
    
    (foreach licenseToken (listDiff inUse LMlicenseList)
;;	     (printf "heh --> \n")
;;	     (printf "--> Knock! Knock! %L \n" (list licenseToken))
	     (lmCheckIn licenseToken))
    (lmFeatList 'LMcheckIn) ))

(defun LMwakeupError (child data)
  (printf "LMwakeupError: %s\n" data)
  t)

(defun LMwakeupStop (child exitStatus)
  (info "LMwakeup exited with %d exit status\n" exitStatus)
  (info "License TIMEOUT no longer enabled for current opus process\n")
  t)

(defun LMstartTimers ()
  (let (timeout)
    (setq timeout LMdefaultTimeout)
    
    ;; We must set PID to a global variable so the process doesn't
    ;; get automatically killed and garbage collected.
    (setq LMprocessID
	  (ipcBeginProcess
	   (sprintf nil "LMwakeup %d" timeout)
	   "" 
	   'LMwakeup 
	   'LMwakeupError 
	   'LMwakeupStop ))
    LMprocessID ))

;;; TREE =====================================

(defun TREEsch (@optional TREElibElemsAlso? TREEprint)
  (let (viewSwitchList (editRep (getEditRep)))
    ;; Make sure a schematic is being edited.
    (cond
     ((or (null editRep) (nequal editRep~>cellViewType "schematic"))
      (printf "You must be editing a schematic!!!\n")
      nil)
     (t
      (cond
       ((member editRep~>viewName '( "schematic"))
	(setq viewSwitchList "schematic"))
       (t
	(setq viewSwitchList (strcat editRep~>viewName "schematic"))))
      (TREEview editRep TREElibElemsAlso? viewSwitchList nil TREEprint) ))))

(defun TREEview (@optional (editRep (getEditRep))
			   TREElibElemsAlso? TREEviewName
			   (TREEincTimestamps nil) 			   
			   (TREEprint t)
			   "dggg" )
  (unless TREEviewName
    TREEviewName = editRep~>viewName )
  (let (TREEglobalList TREEhierList editRepId editRepLibId
		       editRepInfo )
    ;; Extract block hierarchy information
    (printf "Extracting %s hierarchy information...\n" TREEviewName)
    (TREEgetBlockInfo editRep TREEviewName)
    
    ;; update the global count for the edit rep block
    (setq editRepLibId editRep~>lib)
    (setq editRepId (ddGetObj editRepLibId~>name editRep~>cellName))
    (setq editRepInfo
	  ;; assq does not work here
	  (cadr (assoc editRepId
		       (cadr (assoc editRepLibId TREEglobalList)) )))
    (putpropq editRepInfo 1 count)
    
    ;; Print block info
    (if TREEprint 
	(TREEprintResults editRepId TREEviewName) 
      )
    TREEhierList)
   )

(defun daisySchTree (@optional 
		     (editRep (getEditRep))
		     TREElibElemsAlso? TREEviewName
		     (TREEincTimestamps nil)			   
		     "dggg" )
  (unless TREEviewName
    TREEviewName = editRep~>viewName )
  (let (TREEglobalList 
	TREEhierList 
	editRepId editRepLibId
	editRepInfo )

    ;; Extract block hierarchy information
    (printf "Extracting %s hierarchy information...\n" TREEviewName)
    (TREEgetBlockInfo editRep TREEviewName)
    
    ;; update the global count for the edit rep block
    (setq editRepLibId editRep~>lib)
    (setq editRepId (ddGetObj editRepLibId~>name editRep~>cellName))
    (setq editRepInfo
	  (cadr (assoc editRepId
		       (cadr (assoc editRepLibId TREEglobalList)) )))
    (putpropq editRepInfo 1 count)
    TREEhierList)
   )

(defun TREEprintGlobal (outport)
  (let (blockInfo)
    (fprintf outport "\n\nGlobal block listing...\n\n")
    (fprintf outport "=======================\n\n")
    (setq TREEglobalList
	  (sortcar TREEglobalList
		   (lambda (x y) (alphalessp
				  (or x~>name "UNDEFINED")
				  (or y~>name "UNDEFINED") ))))
    (foreach TREEglobalLibList TREEglobalList
	     (rplacd TREEglobalLibList
		     (list (sortcar (cadr TREEglobalLibList)
				    (lambda (x y)
				      (alphalessp
				       (or x~>name "UNDEFINED")
				       (or y~>name "UNDEFINED") ))))))
    (foreach TREEglobalLibList TREEglobalList
	     (fprintf outport "\nLibrary '%s':\n"
		      (or (car TREEglobalLibList)~>name "UNDEFINED") )
	     (foreach block (cadr TREEglobalLibList)
		      (fprintf outport "\t%-10s\t"
			       (or (car block)~>name
				   (cadr block)->name
				   "UNDEFINED"))
		      (setq blockInfo (cadr block))
		      (fprintf outport "[%d]\t" blockInfo->count)
		      (when blockInfo->figure
			(fprintf outport "{%s}" blockInfo->figure))
		      (when TREEincTimestamps
			(let\* ((cn (car block)~>name)
				(ln (car block)~>lib~>name)
				(dbid (dbOpenCellViewByType ln cn TREEviewName))
				)
			       (when dbid
				 (fprintf outport "\t %L"
					  dbid~>instancesLastChanged )
				 (dbClose dbid) )))
		      (fprintf outport "\n")))))

(defun TREEprintHier (outport blockId level)
  (let (spaces spacingFormat sortedCells viewSpace k)
    
    ;; Determine how many spaces to indent the printout
    (sprintf spacingFormat "%%%ds" (times level 3))
    (sprintf spaces spacingFormat ",")
    (setq spaces "")
    (setq viewSpace "")
    (for k 1 level 
	 (setq spaces (strcat spaces ",")))
    (for k 1 20-level
	 (setq viewSpace (strcat viewSpace ",")))
    
    ;; assq does not work here
    (setq sortedCells (copy (cadr (assoc blockId TREEhierList))))
    (setq sortedCells (sort sortedCells 'TREEcmpCells))
    
    
    (foreach blockInfo sortedCells

;;	     (fprintf outport "%L\n" blockInfo->?? )
					 ;;"daisyCommon"

	     (if (and (not (member blockInfo->libName
				   (list "basic"
					 "tsmcN80"
					 "tsmcN40"
					 "tcbn40lpbwphvt_110a"
					 "tcbn40lpbwp_110a"
					 "tsmcN90lo" 
					 "tcbn90ghphvt_150a" 
					 "tpbn90gv_cup"
					 "tcbn80gchvt_111a"
					 "daisy"
					 "gpdk045"
					 "cmos065"
					 "tcbn90ghp_150a"
					 "tcbn80gc_111a"
					 "analogLib"
					 "igwLib")))
		      (not (member blockInfo->name 
				   (list "Title" "daisySheet"))))
		 (progn
		   (cond 
		    ((equal blockInfo->dmId nil)
		     (fprintf outport "%s%s,%-15s (nil) \t" 
			      blockInfo->libName 
			      spaces 
			      blockInfo->name ))
		    (t          
		     (fprintf outport "%s%s,%-15s\t "  
			      blockInfo->libName 
			      spaces
			      blockInfo->name )))
		   
		   (fprintf outport viewSpace)
		   (foreach viewName blockInfo->dmId->views~>name
			    (if (member viewName (list "schematic" "symbol" 
						       "layout" "verilog" 
						       "behaviour" "behavioral" 
						       "veriloga") )
				(fprintf outport ",%s" viewName )))
		   (cond
		    ((greaterp blockInfo->count 10000) 
		     (fprintf outport ",[%d]\n" blockInfo->count) )
		    (t  (fprintf outport "\n")))

		   ))

			   
	     ;; Expand the nested block
	     (when (cadr (assoc blockInfo->dmId TREEhierList))
	       (TREEprintHier outport blockInfo->dmId (plus 1 level)) ))))

(defun TREEcmpCells (left right)
  (let ((leftHier (cadr (assoc left->dmId TREEhierList)))
	(rightHier (cadr (assoc right->dmId TREEhierList)))
	)
    (cond
     ;; test subhier of the cells
     ((and !leftHier rightHier) t)
     ((and leftHier !rightHier) nil)
     ;; both either have hier or both do not: sort by libName
     ((alphalessp left->libName right->libName) t)
     ((alphalessp right->libName left->libName) nil)
     ;; both libNames the same: sort by cellName
     ((alphalessp left->name right->name) t)
     ((alphalessp right->name left->name) nil)
     (t t) )))

(defun TREEgetBlockInfo (currentRep TREEviewName "dt")
  (let (nestedBlockList nestedLibName nestedLibId nestedBlockName
			nestedBlockId nestedBlockInfo nestedBlockCount
			globalBlockInfo isAlreadyGlobal? currentRepName
			currentLibId currentRepId instances )

    ;; Build hierarchy information for the current block
    (foreach instH currentRep~>instHeaders
	     (when (and TREElibElemsAlso? 
			(null instH~>variants)
			(setq instances instH~>instances) )
	       (setq nestedLibName instH~>libName)
	       (setq nestedLibId (or instH~>master~>lib
				     (ddGetObj instH~>libName)))
	       (setq nestedBlockName instH~>cellName)
	       (setq nestedBlockId
		     (ddGetObj nestedLibId~>name nestedBlockName) )
	       
	       ;; If the master of the instance has not been counted then
	       ;; count it.  This will place the block in both the
	       ;; TREEglobalList and the TREEhierList.
	       (setq isAlreadyGlobal?
		     (assoc nestedBlockId
			    (cadr (assoc nestedLibId TREEglobalList)) ))
	       (unless isAlreadyGlobal?
		 (TREEpushIntoBlock (car instances) TREEviewName) )
	       
	       ;; Count repetions represented by this instances:
	       ;;  numInst for schematics,
	       ;;  mosaic row/column for layouts.
	       (setq nestedBlockCount 0)
	       (foreach inst instances
			(cond
			 ;; Handle schematic or plain instance case.
			 ((equal inst~>type "trueInst")
			  nestedBlockCount = nestedBlockCount + inst~>numInst)
			 ;; Error check for unsupported instances types
			 ((nequal inst~>objType "mosaicInst")
			  (warn "TREEgetBlockInfo: skipped unsupported instance type (%L) or objType (%L), instName=%L, in %L\n"
				inst~>type inst~>objType inst~>name
				(list currentRep~>libName currentRep~>cellName
				      currentRep~>viewName )))
			 ((nequal inst~>mosaic~>mosaicType "simple")
			  (warn "TREEgetBlockInfo: skipped unsupported mosaicType (%L), instName=%L, in %L\n"
				inst~>mosaic~>mosaicType inst~>name
				(list currentRep~>libName currentRep~>cellName
				      currentRep~>viewName)))
			 ;; Known Simple Mosaic here
			 (t
			  inst = inst~>mosaic
			  (setq nestedBlockCount
				(plus nestedBlockCount
				      (inst~>rows * inst~>columns) )))))

	       ;; Add the nested block information to the current blocks
	       ;; hierarchy.
	       (cond
		;; The following line didn't work right with missing masters
		;;    ((exists x nestedBlockList (equal x->dmId nestedBlockId))
		((exists x nestedBlockList ((equal x->name nestedBlockName) && (equal x->libName nestedLibName)))
		 ;;		      (exists y nestedBlockList (equal y->libName nestedLibName)))
		 (foreach nestedBlock nestedBlockList
			  (when ((equal nestedBlock->name nestedBlockName) && (equal nestedBlock->libName nestedLibName))
			    (putpropq
			     nestedBlock
			     (plus nestedBlock->count nestedBlockCount)
			     count))))
		(t  (setq nestedBlockInfo
			  (list nil 'name nestedBlockName
				'dmId nestedBlockId
				'libName nestedLibName
				'dmLibId nestedLibId
				'count nestedBlockCount ))
		    (setq nestedBlockList
			  (cons nestedBlockInfo nestedBlockList) )))
	       
	       ;; Determine if the global count for any nested blocks must be
	       ;; updated.
	       (cond
		(isAlreadyGlobal? 
		 (TREEupdateGlobalCountForNestedBlocks
 		  nestedBlockId nestedBlockCount ))
		((greaterp nestedBlockCount 1)
		 (TREEupdateGlobalCountForNestedBlocks
		  nestedBlockId (difference nestedBlockCount 1) )))
	       
	       ;; Increment the number of times the nested block
	       ;; appears globally.
	       ;; assq does not work here
	       (setq globalBlockInfo
		     (cadr (assoc nestedBlockId
				  (cadr (assoc nestedLibId TREEglobalList)) )))
	       globalBlockInfo->count =
	       globalBlockInfo->count + nestedBlockCount ))
    
    ;; Add the current block to the TREEglobalList and the TREEhierList
    (setq currentRepName currentRep~>cellName)
    (setq currentLibId currentRep~>lib)
    (setq currentRepId (ddGetObj currentLibId~>name currentRepName))
    
    (setq TREEglobalLibList (assoc currentLibId TREEglobalList))
    (cond
     (TREEglobalLibList 
      (rplacd TREEglobalLibList 
	      (list (cons (list currentRepId 
				(list nil 'name currentRepName 'count 0
				      'figure currentRep~>figureNumber))
			  (cadr TREEglobalLibList)))))
     (t  (setq TREEglobalLibList
	       (cons (list
		      currentRepId
		      (list nil 'name currentRepName
			    'count 0
			    'figure currentRep~>figureNumber))
		     (cadr TREEglobalLibList)))
	 (setq TREEglobalList
	       (cons (list currentLibId TREEglobalLibList)
		     TREEglobalList))))
    (setq TREEhierList
	  (cons (list currentRepId nestedBlockList) TREEhierList))))

(defun TREEpushIntoBlock (inst TREEviewName)
  (let (blockRep blockName blockLibId blockId TREEglobalLibList)
    
    (setq blockRep
	  (prog1 (dbGetAnyInstSwitchMaster inst TREEviewName)
	    (while (getWarn) nil)))
    (cond
     ((null blockRep)
      (setq blockName inst~>cellName)
      (setq blockLibId (or inst~>master~>lib (ddGetObj inst~>libName)))
      (setq blockId (ddGetObj inst~>libName blockName))
      
      (setq TREEglobalLibList (assoc blockLibId TREEglobalList))
      
      (cond
       (TREEglobalLibList
	(rplacd TREEglobalLibList
		(list (cons
		       (list blockId
			     (list nil 'name blockName
				   'count 0
				   'figure nil))
		       (cadr TREEglobalLibList)))))
       (t  (setq TREEglobalLibList 
		 (cons (list blockId
			     (list nil 'name blockName
				   'count 0
				   'figure nil))
		       (cadr TREEglobalLibList)))
	   (setq TREEglobalList
		 (cons (list blockLibId TREEglobalLibList)
		       TREEglobalList ))))
      (setq TREEhierList (cons (list blockId nil) TREEhierList)) )
     (t (TREEgetBlockInfo blockRep TREEviewName)) )))

(defun TREEupdateGlobalCountForNestedBlocks (currentBlock iterationFactor)
  (foreach blockInfo (cadr (assoc currentBlock TREEhierList))
	   (TREEdoUpdateGlobalCount blockInfo iterationFactor) ))

(defun TREEdoUpdateGlobalCount (blockInfo iterationFactor)
  (let (globalInfo blockId blockLibId)
    
    (setq blockLibId blockInfo->dmLibId)
    (setq blockId (ddGetObj blockLibId~>name blockInfo->name))
    
    ;; Update the global count for this block
    (setq globalInfo (cadr (assoc blockId
				  (cadr (assoc blockLibId TREEglobalList)) )))
    (putpropq globalInfo 
	      (plus globalInfo->count
		    (times iterationFactor blockInfo->count) )
	      count )
    ;; Update the global count for nested blocks
    (TREEupdateGlobalCountForNestedBlocks
     blockId
     (times blockInfo->count iterationFactor) )))

(defun TREEprintResults (blockId TREEviewName)
  (let (fname outport blockName)
    
    (setq blockName blockId~>name)
    (setq fname (makeTempFileName "/tmp/TREEschXXXXXX"))
    (cond
     ((null fname)
      (printf "Could not create temporary file in /tmp\n")
      (setq outport poport) )
     ((null (setq outport (outfile fname)))
      (printf "Could not open '%s' for writing\n" fname)
      (setq fname nil)
      (setq outport poport) ))
    ;; (TREEprintGlobal outport)
    (fprintf outport "\n\nHierarchial block listing...\n\n%s/%s\n\n"
	     blockId~>lib~>name blockName)
    (printf "Printing results\n")
    (TREEprintHier outport blockId 1)
    (when fname
      (close outport)
      (view fname 'default (sprintf nil "%s Tree for %s %s" TREEviewName
				    blockName TREEviewName ))
      (sh (sprintf nil "cp -f %s %s/log/%s.csv" fname (getShellEnvVar "USERAREA") (car (last (parseString fname "/")))))
      (sh (sprintf nil "daisyOpenOoo.sh %s/log/%s.csv &" (getShellEnvVar "USERAREA") (car (last (parseString fname "/")))))
      (deleteFile fname) )))

(defun PPPfindGoldenPortOrder (libName cellName terminals defaultPortOrder)
  (let (sortedPorts
	thisCellView
	tmpPorts
	goldenPorts )
    
    (setq sortedPorts (LISTsortUnique (copy terminals)))
    
    (foreach view (ddGetObj libName cellName)~>views
	     (when (and (nequal view~>name "schematic")
			(nequal view~>name )
			(nequal view~>name "symbol"))
	       (setq thisCellView (dbOpenCellViewByType libName cellName view~>name nil "r"))
	       (when thisCellView
		 (when thisCellView~>portOrder
		   (setq tmpPorts (LISTsortUnique (copy thisCellView~>portOrder)))
		   (when (equal tmpPorts sortedPorts)
		     (setq goldenPorts thisCellView~>portOrder) )))))
        (or goldenPorts defaultPortOrder) ))

(defun PPPdoPlacePowerHier (cellview termWireAssoc
				     @optional (thisIsTopLevel t))
  
  ;; if this is at the top level, initialize the global variable that will
  ;; let us keep track of which cellviews we have already visited.
  (when thisIsTopLevel
    (setq PPPhierVisitList (makeTable "pppHierVisitTable" nil)) )

  ;; first, it must try to descend into the schematic of all instances
  ;; and call this function recursively/hierarchically.
  (foreach inst cellview~>instHeaders
	   (let
	       (readCellviewid editCellviewid)
	     
	     ;; try to open the schematic for this instance.  Do not even try
	     ;; to open it for tech library elements.
	     (when (car inst~>instances)
	       (setq readCellviewid
		     (dbGetAnyInstSwitchMaster (car inst~>instances) "schematic") ))
	     
	     ;; only attempt to descend into it if it can read the schematic
	     ;; and verifies that the schematic is not a library element and
	     ;; it has not visited this element before
	     (when (and readCellviewid
			(null (arrayref PPPhierVisitList readCellviewid)))
	       (setq editCellviewid
		     (dbOpenCellViewByType inst~>libName inst~>cellName readCellviewid~>viewName nil "a"))
	       (cond
		((null editCellviewid)
		 (warn (sprintf nil "PPPdoPlacePowerHier: failed to open '%s' '%s' '%s' for append; cellview not modified.\n"
				inst~>libName inst~>cellName readCellviewid~>viewName)))
		(t 
		 (PPPdoPlacePowerHier editCellviewid termWireAssoc nil) )))))
  
  ;; second, it must add the power pins to this schematic.
  (let (pinsAdded)
    
    (setq pinsAdded
	  (PPPplacePowerPins cellview  ; place power pins on this cellview
			     nil       ; work on entire schematic
			     termWireAssoc
			     t         ; create pins as necessary
			     
			     ;; if it should overwrite existing labels
			     (cadr PPPmainForm->createPins->value) 
			     
			     ;; wire label size
			     (or (atof PPPmainForm->labelSize->value)
				 (PPPgetOption "labelSize")
				 0.05) ; default labels as size 0.05 
			     )
	  )
    ;; rename the nets hierarchically as well
    (PPPrenameTheNets cellview)
    
    ;; only update the symbol when there were pins added
    (when pinsAdded
      
      ;; fourth, it must update the symbol for this schematic
      ;; to include the vpwr & vgnd pins.
      (let (readSymbolId editSymbolId)
	
	(setq readSymbolId
	      (dbOpenCellViewByType cellview~>libName cellview~>cellName
				    "symbol" nil "r"))
	
	(cond
	 ((null readSymbolId)
	  (warn (sprintf nil "PPPdoPlacePowerHier: failed to open '%s' '%s' 'symbol' for read; symbol not modified\n"
			 cellview~>libName cellview~>cellName) ))

	 ;; if it found the symbol cellview
	 (readSymbolId
	  
	  (setq editSymbolId
		(dbOpenCellViewByType cellview~>libName cellview~>cellName
				      "symbol" nil "a"))
	  
	  (cond
	   ((null editSymbolId)
	    (warn (sprintf nil "PPPdoPlacePowerHier: failed to open '%s' '%s' 'symbol' for append; symbol not modified\n"
			   cellview~>libName cellview~>cellName) ))
	   ;; if it can edit the symbol, add the power pins
	   (t
	    (PPPaddPowerToSymbol editSymbolId pinsAdded) )))))))
  
  
  ;; finally, remember that it updated this cellview
  (cond
   ;; if this is the top level, remove the global variable
   (thisIsTopLevel
    (setq PPPhierVisitList 'unbound))
   (t
    (setarray PPPhierVisitList cellview t) )) 
  t)


(defun CYleHiSearch_autoCheckOut ()
  (when lmFeatureStatus("300") != "CHECKED_OUT"
	(lmCheckOut "300" 4.4) )
  (leHiSearch))

(setq lecEditSearchItem '(SearchItem "Search..." 
				     "CYleHiSearch_autoCheckOut()") )

;; veriImportCust() is used to pre define defaults in 
;; Verilog Import Options Form.

(defun veriImportCust ()
  
  (let ()
    (loadContext (strcat (prependInstallPath "etc/context/xlUI.cxt") ) )

    ;; Verilog-XL Setup Environment Customization
    ;; These choices should match online documentation.

    impHdlOptionsForm->filterText->defValue = ""
    impHdlOptionsForm->impHdlTargetLibField->defValue = ""
    impHdlOptionsForm->impHdlRefLibField->defValue = "tech stdcellLib"
    impHdlOptionsForm->impHdlVerDesignField->defValue = ""
    impHdlOptionsForm->impHdlFOptionsFileField->defValue = ""
    impHdlOptionsForm->impHdlVerVCmdFileField->defValue = (prependInstallPath "../verilog/examples/libraries/CDS_GEN_UDPS.v")
    impHdlOptionsForm->impHdlVerYCmdFileField->defValue = ""
    impHdlOptionsForm->impHdlLibExtnField->defValue = ""
    impHdlOptionsForm->impHdlIgnoreModFileField->defValue = ""
    impHdlOptionsForm->impHdlNetlistRadioField->defValue = "schematic"
    ;; Other Choices -  "schematic and functional" OR "netlist" OR "netlist and functional"
    impHdlOptionsForm->impHdlNetlViewNameField->defValue = "netlist"
    impHdlOptionsForm->impHdlFuncViewNameField->defValue = "functional"
    impHdlOptionsForm->impHdlSymbViewNameField->defValue = "symbol"
    impHdlOptionsForm->impHdlLogFileField->defValue = "./verilogIn.log"
    impHdlOptionsForm->impHdlWAField->defValue = "/tmp"
    impHdlOptionsForm->impHdlMapTableFileField->defValue = "./verilogIn.map.tables"
    impHdlOptionsForm->impHdlImpModMatchLibToggleField->defValue =  t
    impHdlOptionsForm->impHdlVerCellModsRadioField->defValue = "Import"
    ;; Other Choices -  "Import As Functional" OR  "Create Symbol Only"
    impHdlOptionsForm->impHdlPowerNetField->defValue = ""
    impHdlOptionsForm->impHdlGroundNetField->defValue = ""
    impHdlOptionsForm->impHdlGlobalSignalField->defValue = ""
    
    ;; VerilogIn : Schematics Generation Options 
    ihdlOptionForm->ihdlOptionsMaxRows->value = 1024
    ihdlOptionForm->ihdlOptionsMaxCols->value = 1024
    ihdlOptionForm->ihdlOptionsFontHeight->value = 0.0625
    ihdlOptionForm->ihdlOptionsLineToLine->value = 0.2
    ihdlOptionForm->ihdlOptionsDensityLevel->value =  0
    ihdlOptionForm->ihdlOptionsCdsThruLibName->value = "basic"
    ihdlOptionForm->ihdlOptionsCdsThruCellName->value = "cds_thru"
    ihdlOptionForm->ihdlOptionsCdsThruViewName->value = "symbol"
    ihdlOptionForm->ihdlOptionsIopinsFlag->value = "Left and Right Sides"
    ;; Other Choices -  "Pin Placement File" OR  "All Sides"
    
    ;; Choose Either t or nil for the following options.
    ihdlOptionForm->ihdlOptionsDumbSch->value =  nil
    ihdlOptionForm->ihdlOptionsNoSquare->value =  t
    ihdlOptionForm->ihdlOptionsMinCrossover->value =  nil
    ihdlOptionForm->ihdlOptionsFastLabels->value =  nil
    ihdlOptionForm->ihdlOptionsNoExtraction->value =  t
    ihdlOptionForm->ihdlOptionsIgnoreExtraPins->value =  t
    ihdlOptionForm->ihdlOptionsVerbose->value =  nil )
  
  (when 
      (and (index (getVersion) "4.4.6") 
	   (ENVgetVal "ldv" "version") 
	   (ENVgetVal "cadence" "toolsInstallDir"))
    impHdlOptionsForm->impHdlVerVCmdFileField->defValue = (strcat (ENVgetVal "cadence" "toolsInstallDir") "/" (ENVgetVal "ldv" "version") "/tools/verilog/examples/libraries/CDS_GEN_UDPS.v") ))

(defun DEregUserTriggers (t_viewType @optional s_appTrigger
				     s_menuTrigger s_postInstallTrigger)
  (let ((infoDPL (deGetAppInfo t_viewType))
	oldApp oldMenu oldPost oldTrig )
    
    ;; mimic deRegUserTriggers here:
    (unless infoDPL
      (error "Can't register user triggers because viewType %s is not registered.\n" viewType))
    (or s_appTrigger s_menuTrigger s_postInstallTrigger
	(error "No user user triggers specified for viewType %s.\n" viewType))
    oldApp = infoDPL->userAppTrigger
    oldMenu = infoDPL->userMenuTrigger
    oldPost = infoDPL->userPostInstallTrigger
    
    ;; process each trigger type
    ;; userTrig - trigger being added
    ;; propnm - DPL property name for trigger being added
    ;; extTrig - Trigger Extension function - this is the Cypress generic trigger function.
    (foreach (userTrig propnm extTrig)
      (list s_appTrigger s_menuTrigger s_postInstallTrigger)
      '(userAppTrigger userMenuTrigger userPostInstallTrigger)
      '(DEuserAppTrigger DEuserMenuTrigger DEuserPostInstallTrigger)
      (cond
       ;; leave old triggers undisturbed if we're not adding anything
	(!userTrig t)
	;; if this is the first trigger, just add it normally, so
	;; we have zero overhead until we need two or more triggers.
	(!(setq oldTrig (get infoDPL propnm))
	  (case propnm
		(userAppTrigger
		 (deRegUserTriggers t_viewType s_appTrigger nil nil))
		(userMenuTrigger
		 (deRegUserTriggers t_viewType nil s_menuTrigger nil))
		(userPostInstallTrigger
		 (deRegUserTriggers t_viewType nil nil s_postInstallTrigger))))
	;; oldTrigger was not our triggerExtension function but an
	;; actual user function, replace by extension, store old&new
	;; If an extTrig is found, assume that extTrig is somewhere in
	;; the calling hierarchy.
	((and (nequal oldTrig extTrig) (null (get infoDPL extTrig)))
	 (putprop infoDPL (list oldTrig userTrig) extTrig)
	 (deUnRegUserTriggers t_viewType)
	 (case propnm
	       (userAppTrigger
	      (deRegUserTriggers t_viewType extTrig oldMenu oldPost))
	       (userMenuTrigger
		(deRegUserTriggers t_viewType oldApp extTrig oldPost))
	       (userPostInstallTrigger
		(deRegUserTriggers t_viewType oldApp oldMenu extTrig))))
	
	;; oldTrigger already is our extension function, just store
	;; the new user function if not there already.
	(!(member userTrig (get infoDPL extTrig))
	  (putprop infoDPL
		   (append1 (get infoDPL extTrig) userTrig)
		   extTrig) )
	;; already registered, return t
	(t t) ))))

(defun DEuserAppTrigger (trigDPL)
  (let ((appDPL (deGetAppInfo trigDPL->viewType)))
    (apply 'and
	   (foreach mapcar userTrig (get appDPL 'DEuserAppTrigger)
		    (cond
		     ;; graceful error if not available
		     ((null (isCallable userTrig))
		      (warn "UserAppTrigger, %L, for %L is not defined or autoloadable.\n"
			    userTrig trigDPL->viewType))
		     (t
		      ;; eval instead of apply to also handle macros
		      (eval (list userTrig 'trigDPL))) )))))

(defun DEuserPostInstallTrigger (trigDPL)
  (let ((appDPL (deGetAppInfo trigDPL->viewType)))
    (apply 'and
	   (foreach mapcar userTrig (get appDPL 'DEuserPostInstallTrigger)
		    (cond
		     ;; graceful error if not available
		     ((null (isCallable userTrig))
		      (warn "UserPostInstallTrigger, %L, for %L is not defined or autoloadable.\n"
			    userTrig trigDPL->viewType))
		     (t
		      ;; eval instead of apply to also handle macros
		      (eval (list userTrig 'trigDPL))) )))))

(defun DEuserMenuTrigger (trigDPL)
  (let ((appDPL (deGetAppInfo trigDPL->viewType)))
    (foreach mapcan userTrig (get appDPL 'DEuserMenuTrigger)
      (cond
       ;; graceful error if not available
	((null (isCallable userTrig))
	 (warn "UserMenuTrigger, %L, for %L is not defined or autoloadable.\n"
	       userTrig trigDPL->viewType))
	(t
	 ;; eval instead of apply to also handle macros
	 (eval (list userTrig 'trigDPL))) ))))

(defun ENVloadDesign ()
  (setq ENVoptions (makeTable "envProgramOptions" nil))
  (ENVsetVal "layout" "gridSpacing" 'float 0.005)
  ;; load the user's program options
  (ENVloadVals ?envFile "~/.userenv")
  (ENVloadVals ?envFile "./local.env")
  
  ;; load the file in ENVoverride environment variable if defined
  (let (overrideName)
    (when (and (isCallable 'getShellEnvVar)
	       (setq overrideName (getShellEnvVar "ENVoverride"))
	       (isReadable overrideName))
      (ENVloadVals ?envFile overrideName))) 
  t)

(defun ENVloadVals (@key envFile
			 (tool nil))
  (let ((inPort (and (isReadable envFile) (infile envFile)))
	nextLine
	index
	dataType
	value
	oldValue
	(validDataTypes '(float int string boolean)) )
    
    ;; loop through reading each line of the input file
    (while (and inPort (setq nextLine (lineread inPort)))
      
      ;; when it received a good line with at least four elements
      (when (and (dtpr nextLine) ((length nextLine) >= 4))
	
	;; Build the index into the array.  This converts the
	;; symbols into strings, and builds the list (progName optName)
	(cond ((symbolp (car nextLine))
	       (setq index (list (sprintf nil "%s" (car nextLine))
				 (sprintf nil "%s" (cadr nextLine)))) )
	      ((and (listp (car nextLine)) (equal 3 (length (car nextLine))))
	       (setq index (list (sprintf nil "%s.%s" (cadar nextLine) (caddar nextLine))
				 (sprintf nil "%s" (cadr nextLine)))))
	      (t
	       (error "ENVloadVals: Can't figure out tool %L\n" (car nextLine)) ))
	
	;; get the datatype
	(setq dataType (car (member (caddr nextLine) validDataTypes)))
	(setq value (nth 3 nextLine))
	
	;; Make sure that this is the program they wanted to load
	(when (or (null tool) (and tool (equal tool (car index))))
	  
	  ;; Make sure that dataType was valid
	  (unless dataType
	    (error "ENVloadVals: Unknown data type for %L\n" nextLine) )
	  
	  ;; Do data type check
	  (when (or (and (equal dataType 'int) (null (integerp value)))
		    (and (equal dataType 'float) (null (floatp value)))
		    (and (equal dataType 'string) (null (stringp value)))
		    (and (equal dataType 'boolean) (null (booleanp value))))
	    (error "ENVloadVals: Data type mismatch for %L\n" nextLine) )
	  
	  ;; Do data type check of preexisting program option
	  (setq oldValue (arrayref ENVoptions index))
	  (when oldValue
	    (unless (equal dataType (cadr oldValue))
	      (error "ENVloadVals: Trying to change data type for %L\n" nextLine)))
	  
	  ;; Add this program option
	  (setarray ENVoptions index (list value dataType)) )))
    (and inPort (close inPort))) t)

(defun ENVgetVal (tool optionName @optional (varType nil))
  (let ((theOption (arrayref ENVoptions (list tool optionName))))
    
    ;; do a sanity check on the data type is requested
    (when varType
      (when (and theOption (nequal varType (cadr theOption)))
	(warn "ENVgetVal: Expected type %s; option set to %L\n" varType theOption) ))
    ;; return the program option
    (car theOption)))

(defun ENVsetVal (tool optionName varType value)
  (when (stringp varType)
    (setq varType (concat varType)) )
  (cond
   ((or (and (equal 'string varType) (stringp value))
	(and (equal 'float varType) (numberp value))
	(and (equal 'int varType) (integerp value))
	(and (equal 'boolean varType) (or (setq value (null (null value))) t)))
    (setarray ENVoptions (list tool optionName) (list value varType))
    value)
   (t
    (warn (sprintf nil "ENVputVal: incompatible type and value (%L)\n"
		   (list tool optionName varType value)))
    nil )))

(defun ENVisVal (tool optionName @optional (varType nil))
  (let (tempVal)
    (setq tempVal (arrayref ENVoptions (list tool optionName)))
    (cond ((and tempVal varType)
	   (equal varType (cadr tempVal)))
	  (t
	   (null (null tempVal))))))

(defun ENVgetVarType (tool optionName)
  (cadr (arrayref ENVoptions (list tool optionName))))
