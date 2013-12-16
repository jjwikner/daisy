/* CCSschTree.il

Group      Custom IC, Cadence Design Systems Ltd.
Date       Oct 30, 1995 
Modified   Jul 27, 2006 

Function to perform the equivalent of the "tree" function
for schematics

There are three entry points in this file:

(CCSschTree [optional key arguments])
    Low level entry point - no form. Just uses arguments to define how and
    where the tree gets created

(CCShdbSchTree [optional key arguments])
    Low level entry point - no form. Just uses arguments to define how and
    where the tree gets created. Similar to CCSschTree, but for
    config views.

(CCShiSchTree)
    Form interface to CCSschTree and CCShdbSchTree.

Extended to support configs in July 2006, as well as allowing
output methods to be supplied, so that output can be sent 
elsewhere in different formats.

***************************************************

SCCS Info: @(#) CCSschTree.il 07/27/06.15:22:48 1.5

*/
/*******************************************************************************
*  DISCLAIMER: The following code is provided for Cadence customers to use at  *
*   their own risk. The code may require modification to satisfy the           *
*   requirements of any user. The code and any modifications to the code may   *
*   not be compatible with current or future versions of Cadence products.     *
*   THE CODE IS PROVIDED "AS IS" AND WITH NO WARRANTIES, INCLUDING WITHOUT     *
*   LIMITATION ANY EXPRESS WARRANTIES OR IMPLIED WARRANTIES OF MERCHANTABILITY *
*   OR FITNESS FOR A PARTICULAR USE.                                           *
*******************************************************************************/

/***************************************************************
*                                                              *
*              (CCScompareCellName inst1 inst2)                *
*                                                              *
*       Comparison function when sorting the instHeaders       *
*                                                              *
***************************************************************/

(defun CCScompareCellName (inst1 inst2)
  (let (result)
       (forall property '(libName cellName viewName)
	       (equal (setq result 
			    (strcmp (dbGet inst1 property)
				    (dbGet inst2 property))) 0))
       (lessp result 0)))

/***************************************************************
*                                                              *
*                  (CCSschTreeOutputElement                    *
*              obj indent instInfo lib cell view               *
*                 boundLib boundCell boundView                 *
*             displaySwitchedView isStoppingView)              *
*                                                              *
*   Generic function to output an element into the tree. Is    *
*   passed a whole bunch of information that could be useful   *
*                    to the output function                    *
*                                                              *
***************************************************************/

(defgeneric CCSschTreeOutputElement 
	    (obj indent instInfo lib cell view 
		 boundLib boundCell boundView
		 displaySwitchedView isStoppingView)
	    nil
	    )

/***************************************************************
*                                                              *
*              (CCSschTreeOutputPush obj indent)               *
*                                                              *
* Generic function called whenever the hierarchy is descended  *
*   in the tree. Allows the output formatter to do something   *
*    if needed. There is no method for port, so normally it    *
*                     doesn't do anything.                     *
*                                                              *
***************************************************************/

(defgeneric CCSschTreeOutputPush 
	    (obj indent) 
	    nil
	    )

/***************************************************************
*                                                              *
*              (CCSschTreeOutputPop obj indent)                *
*                                                              *
* Generic function whenever the hierarchy is ascended. Similar *
*                  to CCSschTreeOutputPush.                    *
*                                                              *
***************************************************************/

(defgeneric CCSschTreeOutputPop 
	    (obj indent) 
	    nil
	    )

/***************************************************************
*                                                              *
*                   CCSschTreeOutputElement                    *
*                                                              *
*  Method specialized on port - is the normal element output   *
*                    for the tree utility.                     *
*                                                              *
***************************************************************/

(defmethod CCSschTreeOutputElement 
  ((obj port) indent instInfo lib cell view 
	      _boundLib _boundCell boundView
	      displaySwitchedView _isStoppingView)
  (let (displayedView formatString)
    (if (and displaySwitchedView boundView)
      (sprintf displayedView " [%s]" boundView)
      (setq displayedView "")
      )
    (sprintf formatString "%%%ds %%s %%s %%s %%s%%s\n"
	     indent)
    (fprintf obj formatString "" 
	     lib
	     cell
	     view
	     instInfo
	     displayedView
	     )
    )
  )

/************************************************************************
*                                                                       *
*           (CCSschTree @key (cellView (geGetEditCellView))             *
*                (viewList "schematic cmos.sch symbol")                 *
*                          (stopList "symbol")                          *
*                             (port poport)                             *
*                           (displayPins nil)                           *
*                              (indent 0))                              *
*                                                                       *
* Recursive function to output a "tree" like display from the specified *
* cellView downwards using the specified view and stop lists. The pins  *
*  can be displayed as well if desired. The indent is for internal use  *
*   only and controls the indent level that this cellView's instances   *
*                            are printed at.                            *
*                                                                       *
************************************************************************/

(defun CCSschTree (@key (cellView (geGetEditCellView)) 
		      (viewList "schematic cmos.sch symbol")
		      (stopList "symbol")
		      (port poport)
		      (displayPins nil)
		      (displaySwitchedView nil)
		      (indent 0))
  (let (sortedHeaders count stopAtList isStoppingView
		      switchedView)
       (setq stopAtList (parseString stopList))
       (setq sortedHeaders 
	     (sort (dbGetq cellView instHeaders) 'CCScompareCellName))
       (foreach header sortedHeaders
		;--------------------------------------------------------
		; count the number of instances
		;--------------------------------------------------------
		(setq count (length
			     (if displayPins 
				 ;---------------------------------------
				 ; if we're displaying pins, count them 
				 ; all regardless 
				 ;---------------------------------------
				 (dbGetq header instances)
				 ;---------------------------------------
				 ; otherwise only count those without 
				 ; purpose pin
				 ;---------------------------------------
				 (setof inst 
					(dbGetq header instances) 
					(nequal (dbGetq inst purpose) "pin")))))
		;--------------------------------------------------------
		; when there was at least one 
		; (so that pins can be filtered out)
		;--------------------------------------------------------
		(when (greaterp count 0)
		      ;--------------------------------------------------
		      ; switch views
		      ;--------------------------------------------------
		      (setq switchedView (dbGetAnyInstSwitchMaster 
					  (car (dbGetq header instances))
					  viewList))
		      (setq isStoppingView
			    (and switchedView 
				 (member (dbGetq switchedView viewName)
					 stopAtList)))
		      (CCSschTreeOutputElement
			port 
			indent
			(sprintf nil "(%d)" count)
			(dbGetq header libName)
			(dbGetq header cellName)
			(dbGetq header viewName)
			(dbGetq header libName)
			(dbGetq header cellName)
			(dbGetq switchedView viewName)
			displaySwitchedView
			isStoppingView)
		      ;--------------------------------------------------
		      ; Recursively expand the hierarchy
		      ;--------------------------------------------------
		      (when switchedView
			    (unless isStoppingView
			      (CCSschTreeOutputPush port indent)
			      (CCSschTree ?cellView switchedView
					 ?viewList viewList
					 ?stopList stopList
					 ?port port
					 ?displayPins displayPins
					 ?displaySwitchedView
					 displaySwitchedView
					 ?indent (plus indent 2))
			      (CCSschTreeOutputPop port indent)
			      )
			    ;--------------------------------------------
			    ; I did try closing the switched cell, but it
			    ; didn't work properly. Have to live with this, 
			    ; I suppose!
			    ;--------------------------------------------
			    )
		      )
		)
       t))

/***************************************************************
*                                                              *
* (CCShdbSchTree @key libName cellName viewName (port poport)  *
*         (displayPins nil) (displaySwitchedView nil)          *
*            (indent 0) (collapseStoppingViews t))             *
*                                                              *
*   Similar to CCSschTree, but works on config views. As a     *
*      result, the lib, cell, and view of the config view      *
*      need to be passed in (as it opens the config). The      *
*       main difference in arguments is the presence of        *
*      the collapseStoppingViews argument. Because there       *
*    may be instance and occurrence bindings in the config,    *
*   we normally have to descend into every occurence in the    *
*    hierarchy. The collapseStoppingViews argument is used     *
*   to only output instances which are at stop points once,    *
*     with a count - but can be turned off to output every     *
*                     instance regardless.                     *
*                                                              *
***************************************************************/

(defun CCShdbSchTree (@key 
		      libName
		      cellName
		      viewName
		      (port poport)
		      (displayPins nil)
		      (displaySwitchedView nil)
		      (indent 0)
		      (collapseStoppingViews t)
		      )
  (let (configId pathVectorStack configBag)
    (setq configBag (hdbCreateConfigBag))
    (setq configId (hdbOpen libName cellName viewName "r"))
    (setq pathVectorStack (hdbCreatePathVectorStack))
    (if configId
      (progn
	(CCShdbSchTreeTraverseConfig
	  ?port port
	  ?displayPins displayPins
	  ?displaySwitchedView displaySwitchedView
	  ?indent indent
	  ?pathVectorStack pathVectorStack
	  ?configId configId
	  ?configBag configBag
	  ?collapseStoppingViews collapseStoppingViews
	  )
	(hdbCloseConfigsInBag configBag)
	)
      (error "Could not open config %Y/%Y/%Y\n" libName cellName viewName)
      ) ; if
    ) ; let
  ) ; defun CCShdbSchTree

/*****************************************************************
*                                                                *
*                 (CCShdbSchTreeTraverseConfig                   *
* @key (port poport) (displayPins nil) (displaySwitchedView nil) *
*         (indent 0) pathVectorStack configId configBag          *
*                   (collapseStoppingViews t))                   *
*                                                                *
*   Given an open config, path vector stack, and a config bag,   *
*  traverse the config. This is called from CCShdbSchTree, but   *
*   may also be called from CCShdbSchTreeTraverseCellView if     *
*         a config is switched into from another config.         *
*                                                                *
*****************************************************************/

(defun CCShdbSchTreeTraverseConfig (@key 
				    (port poport)
				    (displayPins nil)
				    (displaySwitchedView nil)
				    (indent 0)
				    pathVectorStack
				    configId
				    configBag
				    (collapseStoppingViews t)
				    )
  (let (libName cellName viewName pathVector)
    (hdbAddConfigToBag configBag configId)
    ;--------------------------------------------------------------------
    ; Find out the cellView associated with the config
    ;--------------------------------------------------------------------
    (setq libName (hdbGetTopLibName configId))
    (setq cellName (hdbGetTopCellName configId)) 
    (setq viewName (hdbGetTopViewName configId)) 
    ;--------------------------------------------------------------------
    ; Create a path vector to manage the hierarchy traversal, and 
    ; then push it onto the stack 
    ;-------------------------------------------------------------------- 
    (setq pathVector (hdbCreatePathVector configId))
    (hdbPushPathVect pathVectorStack pathVector) 
    ;-------------------------------------------------------------------- 
    ; Traverse the cellView 
    ;--------------------------------------------------------------------
    (CCShdbSchTreeTraverseCellView
      ?libName libName
      ?cellName cellName
      ?viewName viewName
      ?port port
      ?indent indent
      ?displayPins displayPins
      ?displaySwitchedView displaySwitchedView
      ?pathVectorStack pathVectorStack
      ?configBag configBag
      ?collapseStoppingViews collapseStoppingViews
      )
    ;--------------------------------------------------------------------
    ; Cleanup. All the configs will be closed at the top level
    ;--------------------------------------------------------------------
    (hdbPopPathVect pathVectorStack)
    (hdbDestroyPathVector pathVector)
    ) ; let
  ) ; defun CCShdbSchTreeTraverseConfig

/*************************************************************************
*                                                                        *
*                    (CCShdbSchTreeTraverseCellView                      *
*              @key libName cellName viewName (port poport)              *
*         (displayPins nil) (displaySwitchedView nil) (indent 0)         *
*          pathVectorStack configBag (collapseStoppingViews t))          *
*                                                                        *
*        The function which does all the work for cellViews under        *
*     configs. This uses pcdb to iterate over the instance masters,      *
*    and then the instances (it can iterate over both the structural     *
* instances, plus any "ignored" instances like pins, taps, sheets, etc). *
*              Recursively descends through the hierarchy.               *
*                                                                        *
*************************************************************************/

(defun CCShdbSchTreeTraverseCellView (@key
				      libName
				      cellName
				      viewName
				      (port poport)
				      (displayPins nil)
				      (displaySwitchedView nil)
				      (indent 0)
				      pathVectorStack
				      configBag
				      (collapseStoppingViews t)
				      )
  (let (pathVector pcId instMasterGens instMaster instGen inst
		   masterLibName masterCellName masterViewName
		   fixedLib fixedCell fixedView instName bindInfo
		   boundLib boundCell boundView 
		   isStoppingView deferredInfo newInfo instInfo
		   expandMasterGens deferredCount)
    (setq pathVector (hdbGetPathVectorStackTop pathVectorStack))
    (setq pcId (pcdbOpen libName cellName viewName "r"))
    (if pcId
      ; then
      (progn
	;----------------------------------------------------------------
	; Create a list of one or two inst master generators, depending
	; on whether pins (i.e. "ignored" instances) are traversed. A
	; second list is created indicating whether expansion should 
	; happen or not for that masterGen - it shouldn't happen for
	; the "ignored" instances.
	;----------------------------------------------------------------
	(setq instMasterGens
	      `(,(pcdbGetInstMasterGen pcId)
		 ,@(and displayPins (list (pcdbGetIgnoredInstMasterGen pcId)))
		 ))
	(setq expandMasterGens `(t ,@(and displayPins (list nil))))
	;----------------------------------------------------------------
	; Loop over the one or two inst master generators
	;----------------------------------------------------------------
	(foreach
	  (instMasterGen expandMasterGen) instMasterGens expandMasterGens
	  (setq instMaster (pcdbNextInstMaster instMasterGen))
	  (while instMaster
		 ;-------------------------------------------------------
		 ; Gather information about the master
		 ;-------------------------------------------------------
		 (setq masterLibName (pcdbInstMasterLib instMaster))
		 (setq masterCellName (pcdbInstMasterCell instMaster))
		 (setq masterViewName (pcdbInstMasterView instMaster))
		 (setq fixedLib (pcdbInstMasterLibFixed instMaster))
		 (setq fixedCell (pcdbInstMasterCellFixed instMaster))
		 (setq fixedView (pcdbInstMasterViewFixed instMaster))
		 ;-------------------------------------------------------
		 ; Now traverse the instances
		 ;-------------------------------------------------------
		 (setq instGen (pcdbGetInstGen instMaster))
		 (setq inst (pcdbNextInst instGen))
		 (while inst
			(setq instName (pcdbInstName inst))
			(setq bindInfo
			      (and expandMasterGen
				   (hdbBind 
				     pathVector
				     masterLibName
				     masterCellName
				     masterViewName
				     instName
				     fixedLib
				     fixedCell
				     fixedView
				     t
				     )))
			(setq boundLib (car bindInfo))
			(setq boundCell (cadr bindInfo))
			(setq boundView (caddr bindInfo))
			;------------------------------------------------
			; Only push if the view was bound
			;------------------------------------------------
			(if (and boundLib boundCell boundView)
			  (progn
			    (hdbPushCell 
			      pathVector instName boundLib boundCell boundView)
			    (setq isStoppingView (hdbIsAtStopPoint pathVector))
			    )
			  ;----------------------------------------------
			  ; Otherwise pretend it is a stopping view to prevent
			  ; hierarchy expansion
			  ;----------------------------------------------
			  (setq isStoppingView t)
			  )
			(when collapseStoppingViews
			  ;----------------------------------------------
			  ; This is a collection of data used to see if
			  ; this instances is the same as the deferred instance
			  ;----------------------------------------------
			  (setq newInfo
				(list 
				  instName 
				  masterLibName masterCellName masterViewName
				  boundLib boundCell boundView))
			  ;----------------------------------------------
			  ; Output the deferred instance information if
			  ; this is either not a stopping view, or it
			  ; is different from the deferred instance
			  ;----------------------------------------------
			  (when
			    (and
			      (or (null isStoppingView)
				  (nequal (cdr newInfo) (cdr deferredInfo)))
			      deferredInfo)
			    (if (greaterp deferredCount 1)
			      (sprintf instInfo "(%d)" deferredCount)
			      (setq instInfo (car deferredInfo)))
			    (CCSschTreeOutputElement
			      port 
			      indent
			      instInfo
			      (nth 1 deferredInfo)
			      (nth 2 deferredInfo)
			      (nth 3 deferredInfo)
			      (nth 4 deferredInfo)
			      (nth 5 deferredInfo)
			      (nth 6 deferredInfo)
			      displaySwitchedView
			      t
			      ) 
			    ) ; when
			  ;----------------------------------------------
			  ; Now either update the count, or record the 
			  ; deferred instance, or discard the current data
			  ; about the deferred instance
			  ;----------------------------------------------
			  (cond
			    ((and isStoppingView
				  (equal (cdr newInfo) (cdr deferredInfo)))
			     (postincrement deferredCount))
			    (isStoppingView
			      (setq deferredInfo newInfo)
			      (setq deferredCount 1)
			      )
			    (t
			      (setq deferredInfo nil)
			      )
			    ) ; cond
			  ) ; when
			;------------------------------------------------
			; Output normally if not collapsing, or this is
			; not a stopping view
			;------------------------------------------------
			(unless (and collapseStoppingViews isStoppingView)
			  (CCSschTreeOutputElement
			    port 
			    indent
			    instName
			    masterLibName
			    masterCellName
			    masterViewName
			    boundLib
			    boundCell
			    boundView
			    displaySwitchedView
			    isStoppingView
			    )
			  ) ; unless
			;------------------------------------------------
			; Now descend if necessary
			;------------------------------------------------
			(unless isStoppingView
			  (CCSschTreeOutputPush port indent)
			  (if (hdbIsConfig boundLib boundCell boundView)
			    ;--------------------------------------------
			    ; If a sub-config, traverse that
			    ;--------------------------------------------
			    (let (configId)
			      (setq configId 
				    (hdbOpen libName cellName viewName "r"))
			      (CCShdbSchTreeTraverseConfig
				?port port
				?displayPins displayPins
				?displaySwitchedView displaySwitchedView
				?indent (plus indent 2)
				?pathVectorStack pathVectorStack
				?configId configId
				?configBag configBag
				?collapseStoppingViews collapseStoppingViews
				)
			      )
			    ;--------------------------------------------
			    ; Otherwise traverse the bound cellView
			    ;--------------------------------------------
			    (CCShdbSchTreeTraverseCellView
			      ?libName boundLib
			      ?cellName boundCell
			      ?viewName boundView
			      ?port port
			      ?indent (plus indent 2)
			      ?displayPins displayPins
			      ?displaySwitchedView displaySwitchedView
			      ?pathVectorStack pathVectorStack
			      ?configBag configBag
			      ?collapseStoppingViews collapseStoppingViews
			      )
			    ) ; if
			  (CCSschTreeOutputPop port indent)
			  ) ; unless isStoppingView
			;------------------------------------------------
			; Pop back up again
			;------------------------------------------------
			(when (and boundLib boundCell boundView)
			  (hdbPopCell pathVector)
			  )
			(setq inst (pcdbNextInst instGen))
			) ; while inst
		 ;-------------------------------------------------------
		 ; Output any deferred instances from collapsing that
		 ; are left over before moving onto the next master
		 ;-------------------------------------------------------
		 (when deferredInfo
		   (if (greaterp deferredCount 1)
		     (sprintf instInfo "(%d)" deferredCount)
		     (setq instInfo (car deferredInfo)))
		   (CCSschTreeOutputElement
		     port 
		     indent
		     instInfo
		     (nth 1 deferredInfo)
		     (nth 2 deferredInfo)
		     (nth 3 deferredInfo)
		     (nth 4 deferredInfo)
		     (nth 5 deferredInfo)
		     (nth 6 deferredInfo)
		     displaySwitchedView
		     t
		     ) 
		   (setq deferredInfo nil)
		   )
		 ;-------------------------------------------------------
		 ; Move to next instance master
		 ;-------------------------------------------------------
		 (setq instMaster (pcdbNextInstMaster instMasterGen))
		 ) ; while instMaster
	  ) ; foreach instMasterGen
	(pcdbClose pcId)
	) ; progn 
      ; else
      (progn
	(error "Couldn't open pcdb for %Y.%Y:%Y\n" libName cellName viewName)
	)
      ) ; if
    ) ; let
  ) ; defun


/***************************************************************
*                                                              *
*                  (CCScreateHiSchTreeForm)                    *
*                                                              *
*       Create the form for the schematic tree function        *
*                                                              *
***************************************************************/

(defun CCScreateHiSchTreeForm ()
  (let (topOrCurrent viewList stopList displayPins displaySwitchedView)
       (setq topOrCurrent 
	     (hiCreateRadioField
	      ?name 'topOrCurrent
	      ?prompt "Starting cell"
	      ?choices '("current" "top")))
       (setq viewList
	     (hiCreateStringField
	      ?name 'viewList
	      ?prompt "View list"
	      ?value "schematic cmos.sch symbol"))
       (setq stopList
	     (hiCreateStringField
	      ?name 'stopList
	      ?prompt "Stop list"
	      ?value "symbol"))
       (setq displayPins
	     (hiCreateBooleanButton
	      ?name 'displayPins
	      ?buttonText "Display pins"
	      ?value nil))
       (setq displaySwitchedView
	     (hiCreateBooleanButton
	      ?name 'displaySwitchedView
	      ?buttonText "Display view used"
	      ?value nil))
       (setq CCShiSchTreeForm
	     (hiCreateAppForm
	      ?name 'CCShiSchTreeForm
	      ?formTitle "Schematic Tree"
	      ?callback 'CCShiSchTreeCB
	      ?fields (list
		       topOrCurrent viewList stopList displayPins
		       displaySwitchedView)))
       CCShiSchTreeForm))

/***************************************************************
*                                                              *
*                    (CCShiSchTreeCB form)                     *
*                                                              *
*            Callback for the schematic tree form.             *
*      Opens the file, outputs the header information and      *
*    then calls CCSschTree to recursively descend down the     *
*                          hierarchy.                          *
*         Then the output file is viewed and deleted.          *
*                                                              *
***************************************************************/

(defun CCShiSchTreeCB (form)
  (let (cellView oport fileName)
       (setq cellView
	     (if (equal (getq (getq form topOrCurrent) value) "top")
		 (geGetTopLevelCellView (getq form treeWindow))
		 (geGetEditCellView (getq form treeWindow))))
       (if cellView
	   (progn
	    (setq oport (outfile (setq fileName 
				       (makeTempFileName "/tmp/schTree"))))
	    (if oport
		(progn
		 (fprintf oport "%40s\n","Design Hierarchy")
		 (fprintf oport 
		  "*******************************************************\n")
		 (fprintf oport "Library : %s\n" (dbGetq cellView libName))
		 (fprintf oport "Cell    : %s\n" (dbGetq cellView cellName))
		 (fprintf oport "View    : %s\n" (dbGetq cellView viewName))
		 (fprintf oport "Option  : %s to bottom\n" 
			  (getq (getq form topOrCurrent) value))
		 (fprintf oport 
		  "*******************************************************\n\n"
		  )
		 (CCSschTree ?cellView cellView
			    ?viewList (getq (getq form viewList) value)
			    ?stopList (getq (getq form stopList) value)
			    ?displayPins (getq (getq form displayPins) value)
			    ?displaySwitchedView 
				(getq (getq form displaySwitchedView) value)
			    ?port oport
			    )
		 (close oport)
		 (view fileName nil "Tree")
		 (deleteFile fileName)
		 )
		(error "Couldn't open output file"))
	    )
	   (error "Couldn't find cellView"))
       ))

/********************************************************************
*                                                                   *
*                   (CCScreateHiHdbSchTreeForm)                     *
*                                                                   *
* Since the form fields for expanding a config view are pretty much *
* completely different from a normal view, create a separate form.  *
*               Easier than having the form dynamic.                *
*                                                                   *
********************************************************************/

(defun CCScreateHiHdbSchTreeForm ()
  (let (displayPins displaySwitchedView collapseStoppingViews)
       (setq displayPins
	     (hiCreateBooleanButton
	      ?name 'displayPins
	      ?buttonText "Display pins"
	      ?value nil))
       (setq displaySwitchedView
	     (hiCreateBooleanButton
	      ?name 'displaySwitchedView
	      ?buttonText "Display view used"
	      ?value nil))
       (setq collapseStoppingViews
	     (hiCreateBooleanButton
	      ?name 'collapseStoppingViews
	      ?buttonText "Collapse stopping views"
	      ?value t))
       (hiCreateAppForm
	 ?name 'CCShiHdbSchTreeForm
	      ?formTitle "Schematic Tree from config"
	      ?callback 'CCShiHdbSchTreeCB
	      ?fields (list
		       displayPins
		       displaySwitchedView
		       collapseStoppingViews))
       ) ; let
  ) ; defun

/****************************************************************
*                                                               *
*                   (CCShiHdbSchTreeCB form)                    *
*                                                               *
*   Form callback for config tree expansion. Opens the file,    *
* writes out the header, and then calls the expansion function. *
*       Then closes the file and displays it in a window.       *
*                                                               *
****************************************************************/

(defun CCShiHdbSchTreeCB (form)
  (let (config oport fileName)
    (setq config (getq form config))
    (setq oport (outfile (setq fileName 
			       (makeTempFileName "/tmp/schTree"))))
    (if oport
      (progn
	(fprintf oport "%40s\n","Design Hierarchy")
	(fprintf oport 
		 "*******************************************************\n")
	(fprintf oport "Library     : %s\n" (hdbGetLibName config))
	(fprintf oport "Cell        : %s\n" (hdbGetCellName config))
	(fprintf oport "View        : %s\n" (hdbGetViewName config))
	(fprintf oport "Top Library : %s\n" (hdbGetTopLibName config))
	(fprintf oport "Top Cell    : %s\n" (hdbGetTopCellName config))
	(fprintf oport "Top View    : %s\n" (hdbGetTopViewName config))
	(fprintf oport 
		 "*******************************************************\n\n"
		 )
	(CCShdbSchTree 
	  ?libName (hdbGetLibName config)
	  ?cellName (hdbGetCellName config)
	  ?viewName (hdbGetViewName config)
	  ?displayPins (getq (getq form displayPins) value)
	  ?displaySwitchedView (getq (getq form displaySwitchedView) value)
	  ?collapseStoppingViews (getq (getq form collapseStoppingViews) value)
	  ?port oport
	  )
	(close oport)
	(view fileName nil "Tree")
	(deleteFile fileName)
	)
      (error "Couldn't open output file"))
    ) ; let
  ) ; procedure

/***************************************************************
*                                                              *
*                       (CCShiSchTree)                         *
*                                                              *
*        Display a form controlling the tree generation        *
*                                                              *
***************************************************************/

(defun CCShiSchTree ()
  (let (wid config)
    (setq wid (hiGetCurrentWindow))
    (setq config (deGetConfigId wid))
    (if config
      ; then 
      (progn
	;----------------------------------------------------------------
	; create the form if it doesn't exist
	;----------------------------------------------------------------
	(unless (and (boundp 'CCShiHdbSchTreeForm) CCShiHdbSchTreeForm)
	  (CCScreateHiHdbSchTreeForm))
	(putpropq CCShiHdbSchTreeForm config config)
	(hiDisplayForm CCShiHdbSchTreeForm)
	)
      ; else
      (progn
	;----------------------------------------------------------------
	; create the form if it doesn't exist
	;----------------------------------------------------------------
	(unless (and (boundp 'CCShiSchTreeForm) CCShiSchTreeForm)
	  (CCScreateHiSchTreeForm))
	;----------------------------------------------------------------
	; store the window on the form so that it will always use the window
	; that was current when the command was invoked
	;----------------------------------------------------------------
	(putpropq CCShiSchTreeForm wid treeWindow)
	(hiDisplayForm CCShiSchTreeForm)
	)
      ) ; if
    ) ; let
  ) ; defun

