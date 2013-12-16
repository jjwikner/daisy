;; ==

;;; **************************
;;; Create a pad frame layout for xStuck full custom STCMOS065 chip, ES/ISY, LiU.
;;; **************************
;; Author : Dr. J Jacob Wikner
;; Revised: Syed A. Aamir
;; 

;; version history
;; PA5 - additions 
;; * tackling AP layer merge by changing to correct "layout_40u"
;; * bondpad should not be added as cell already contains it.




(defun ltf ()
  (printf "==================> Loading the daisyGenPadLayout \n")
  (loadi "xStuckTop/skill/daisyGenPadLayout8.il")
)



;; Inherit this from master wrapper
(setq pathName     "/xStuckTop/")

;; rev 57 is a new port pin version
;; rev 59 is a special case

(setq rev "p57b")


(setq genIOPinsOnly nil)
(setq workLibName      "xStuckIos")
(setq cellView         "xStuckIosTop")

(setq fileName 
      (strcat 
       "io_definition"
       "_"
       cellView
       "_"
       rev))

(setq basePath     (strcat (getShellEnvVar "PROJAREA") pathName))
(setq sklPath      (strcat basePath  "/skill/" ))

;; Declare some local variables

;;(setq padHeight 168.400) 
;;(setq padWidth   30)

;; IO pad
(setq padHeight  112) 
(setq padWidth   40)

(setq verticalOffset 894.4)

;; Corner (112um x 112um ) 


(setq ignorePadSurfaces (list "BD8SCARUDQP_2V5_SF_LIN"
			      "VDD_ST_SF_LIN"
			      "GND_ST_SF_LIN"  
			      "VDDE_ST_SF_LIN" 
			      "GNDE_ST_SF_LIN" 
			      "ANA_2V5_SF_LIN"
			      "someOtherSTsCrapPad" 
			       ))


;(setq masterFillerCell
;      (dbOpenCellViewByType  
;       "IO65LP_SF_BASIC_50A_ST_7M4X0Y2Z" 
;       "IOFILLERCELL_I_ST_SF_LIN"
;       "layout"))

(setq masterFillerCell
      (dbOpenCellViewByType  
       "IO65LP_SF_BASIC_50A_ST_7M4X0Y2Z" 
       "IOFILLERCELL32_ST_SF_LIN"
       "layout"))




;; library: IO65LPHVT_ANA_50A_7M4X0Y2Z
;; filler cell: "IOFILLERCELL_B2B_ANA_LIN"
;;              "layout"


(setq originP (list 0.0 0.0))
(setq upperXY (list 100.0 100.0))

;;  =========================================

(setq docFileBase  (strcat basePath  "/doc/"   fileName ))
(setq logFileBase  (strcat basePath  "/log/"   fileName ))

;; Open the input reference file
(setq padList (readCSVfile (strcat docFileBase ".csv")))

;; Open the output log files
(setq logPort  (outfile (strcat logFileBase ".log")))
(setq posPort  (outfile (strcat logFileBase "_placement.csv")))
(setq padsPort  (outfile (strcat logFileBase "_pads.csv")))

(fprintf posPort "%s,X (pad),Y (pad),X (plcmnt), Y (plcmnt),UniqueNo\n" (car (csvAlist (car padList) "," "")))

;; Setup the design path
(setq deleteAll t)
(setq rodPinNameList   (list ))

;; Open the cell view where the pad frame is to be created
;; creates the "layout" viewname,  if genIOPinsOnly is not set. 

(setq padTopCellId (dbOpenCellViewByType 
		    workLibName cellView 
		    (if (not genIOPinsOnly) (setq viewType "layout") 
		      (setq viewType "layoutPins")) "maskLayout" "w"))

;; gets a list of opened windows and if coincides with ours (layout_X), then assigns it openWinId
;; a foreach is really needed?


(setq winId nil)
(foreach openWinId (hiGetWindowList)
	 (if (equal (getq openWinId cellView) padTopCellId)
	     (setq winId openWinId)))

(if (not winId) (setq winId (hiOpenWindow)))

(geOpen ?window winId
	?lib workLibName
	?cell cellView
	?view viewType
	?viewType "maskLayout"
	?mode "a")

;;initial clean up

(if deleteAll 
    (progn
      (geSelectAllFig padTopCellId)  ;; selecting (all objects) padTopCellId, geGetSelectedSet will return
      (if (geGetSelectedSet)         ;; (c) - list of all selected
	  (leHiDelete winId))        ;; deletes objects in winId
      (dbDeleteAllNet padTopCellId)
      
    )
)  ;; deletes all nets from padTopCellId

(setq dirList 
      (list 
       (list "input" "output" 		
	     (dbOpenCellViewByType "basic" "ipin" "symbol" "" 'r ) )
       (list "output" "input" 		
	     (dbOpenCellViewByType "basic" "opin" "symbol" "" 'r ) )
       (list "inputOutput" "inputOutput" 		
	     (dbOpenCellViewByType "basic" "iopin" "symbol" "" 'r ) )))


;; Enable a couple of layers.
(leSetLayerValid (list "instance" "drawing") t)
(leSetLayerValid (list "text"     "drawing") t)

;; Rotation transform list
(setq rotTransform 
      (list 
       (list "R0"   "R90"  0 -1) 
       (list "R90"  "R180" 1  0) 
       (list "R180" "R270" 0  1) 
       (list "R270" "R0"  -1  0)))

(setq rotLabel
      (list 
       (list "R0"   "R90"  1 -1 "Right") 
       (list "R90"  "R0"   0  1 "Left") 
       (list "R180" "R90" -1  1 "Left") 
       (list "R270" "R0"   0 -1 "Right")))

(setq xPos 0)
(setq yPos 0)
(setq oldCell nil)
(setq noP  1)
(setq noNum  0)
(setq cornerNum  -1)
(setq direction "inputOutput")
(setq lRot "R0")
(setq rot  "R270")


(setq countIndex 0)

;; Generating the Layout View of the Pad Frame
(foreach padObject padList
	 
	 ;; Special case used for corners ===================================================
	 ;; No pins are added for the corners ================================================
	
	 ; (setq padObject (car padList))

	 (if (equal (assoCadr padObject "Pinname") "CORNER")
	     (progn
	       ;; Compute corner offset
	       (setq padLibName  (assoCadr padObject "Library") )
	       (setq padCellName (assoCadr padObject "Cellname"))
	 
		;; layout_40u should be replaced with another assoCadr
	       ;; picks the library, cellview and name "layout" of pad from spreadsheet.
	       (setq newCell (dbOpenCellViewByType 
			      padLibName padCellName 
			      
			      (if (equal (assoCadr padObject "Pinname") "CORNER")
				  "layout"     ;; if CORNER then layout
				"layout_40u")  ;; else takes layout_40u
			     
			      ))

	       ; replace with case statements please ! 
	       
	       (if (equal rot "R90")   (setq yPos (plus yPos padHeight)))

	       (if (equal rot "R270")  (setq yPos (plus yPos -padHeight)))

	       (if (equal rot "R0")    (setq xPos (plus xPos padHeight)))

	       (if (equal rot "R180")  (setq xPos (plus xPos -padHeight)))
	       
	       ;; find the  position for the new cell
	       
	       (setq newPos (getInstanceSpace (list xPos yPos) rot newCell oldCell))
	       
	       (setq xPos (car newPos))
	       (setq yPos (cadr newPos))
	       
	       (setq oldCell  newCell)
	       
	       ;; set the name for the new instance
	       ;; If it is a corner, or else no, define instName accordingly -- seems like a redundant check
	       ;; arnt we already in the corner case - maybe Nr. checking Nr. is required

	       (if (equal (assoCadr padObject "Nr.") "nil" )
		   
		   (progn
		     (sprintf instName "%s_%d" 
			      (assoCadr padObject "Pinname") 
			      (preincrement cornerNum)) ) ;; progn ends here		 

      		 (sprintf instName "Ipad_%s%s" 
			  (assoCadr padObject "Pinname") 
			  (assoCadr padObject "Nr."))
		 )  ;;  if ends here 
	       
	       ;; Place an instance of the (pad)cell into opened cellview
	       (setq figId 
		     (dbCreateInst 
		      padTopCellId 
		      newCell
		      instName
		      newPos  
		      (cadr (assoc rot rotTransform))))   ;; selects "R0" in the first run, why ?
	       
	       ;; setting the variables according to corner no. (0 for southwest, 2 for northeast )

	       (if (equal cornerNum 0)
		   (setq originP newPos) )

	       (if (equal cornerNum 2)
		   (setq upperXY newPos) )
	       
	        ;; writing to posPort all the values of 'entered' values for padObject, along with xPos,yPos 

	       (fprintf posPort "%s,%3.3f,%3.3f,,\n" 
			(cadr (csvAlist padObject "," ""))
			(float xPos) (float yPos) )       ;; fprintf ends 
	       
	       (setq oldCell nil)

	       ;; Uppdate the rotation selector
	       (setq rot (cadr (assoc rot rotTransform)))
	       
	       ;; Update the position variables to fit the offset within the corner cell
	       ;; (replace with case statment)

	       (if (equal rot    "R0")
		   (progn
		     (setq xPos xPos+(caadr (daisyBoundaryBox newCell)))
		     (setq lRot "R90") ))

	       (if (equal rot    "R180") 
		   (progn 
		     (setq xPos xPos-(caadr  (daisyBoundaryBox newCell)))
		     (setq lRot "R90") ))

	       (if (equal rot    "R90" ) 
		   (progn 		     (setq yPos yPos+(cadadr (daisyBoundaryBox newCell)))
		     (setq lRot "R0") ))

	       (if (equal rot    "R270") 
		   (progn 
		     (setq yPos yPos-(cadadr (daisyBoundaryBox newCell)))
		     (setq lRot "R0") ))
	       
	       ) ;; ends progn
	   ;; END OF CORNER INSERTION ===============================================================================
	   
	   ;; =================================================================================
	   
	   ;;; if not "CORNER" then flow comes to execute this progn

	   (progn
	     ;; Open the new cell and examine its height to adjust pin spacing to cell height	     
	     
	     ;; setting padLibName again; 
	     ;; if dummyLib has a value other than "nil", than that is taken forward, 
	     ;; otherwise the main "Library" is chosen as usual
	   
	     (setq padLibName (assoCadr padObject 
				(if (not (equal (assoCadr padObject "DummyLib") "nil"))  
				 "DummyLib" "Library"))) ;; setq closes
	     
	     ;; OR (setq padLibName (assoCadr padObject "Library"))
	     
	     ;; setting padCellName again; 
	     ;; if dummyCell has a value other than "nil", than that is taken forward,
	     ;; otherwise the main "Cellname" is chosen as usual
	   
	     (setq padCellName 
		   (assoCadr padObject 
			     (if (not (equal (assoCadr padObject "DummyCell") "nil"))   
				 "DummyCell" "Cellname"))) ;; setq closes
	     
	      ;; OR (setq padCellName (assoCadr padObject "Cellname"))

	     ;; As i understand "Nr." is the real pin number omitting (if "nil") the corner/corner pushing 
	     ;; (c) fillers/Gnd-to-Gnd diodes, which dont come in normal cells
	     ;; However an even check looks strange -> could be removed/could be made useful for pins
	     ;; All the conditions pick layout_50u which should be revised upon confirmation.

	     (setq newCell     
		   (dbOpenCellViewByType 
		    padLibName padCellName 
		    (if (numberp (evalstring (assoCadr padObject "Nr.")))
			(if (evenp (evalstring (assoCadr padObject "Nr.")))
			    "layout_40u"
			  "layout_40u")
		      "layout") ;; first if closed
		   ) ;; dbOpen.. closes
		   ) ;;setq closes
	     
	     ;;(defun getInstanceSpace (oldPos rot newCell oldCell @optional (boundary t))

	     (setq newPos (getInstanceSpace (list xPos yPos) rot newCell oldCell))
	     (setq xPos (car newPos))
	     (setq yPos (cadr newPos))	   	     
	     

     	     ;; Set the name for the new instance
	     (if (equal (assoCadr padObject "Nr.") "nil")    ;; this collides !!  as only CORNERS are nil !
		 (sprintf instName "%s_NoNum_<%d>" 
			  (assoCadr padObject "Pinname") (preincrement noNum));;sprintf closed

	     (sprintf instName "Ipad_%s%s" 
		 (assoCadr padObject "Pinname") 
		 (assoCadr padObject "Nr.")) ) ;; if closed
	     
	     ;;(fprintf padsPort "%3.1f,%3.1f,,"  (float xPos)(float yPos))
	    ; (printf "%s,x=%3.1f,y=%3.1f, \n"  (assoCadr padObject "Pinname")  (float xPos) (float yPos) + (abs (cadr originP)))
    
	     ;;(fprintf padsPort "%s,%s,%3.1f,%3.1f, \n" (assoCadr padObject "Nr.") (assoCadr padObject "Pinname") (float xPos) ((float yPos) + (abs (cadr originP))) )
	     (fprintf padsPort "%s,%s,%3.1f,%3.1f, \n" (assoCadr padObject "Nr.") (assoCadr padObject "Pinname") (float xPos) ((float yPos) + (float verticalOffset) ) )

	    ;; (fprintf padsPort "%s,%3.3f,%3.3f,,\n" 
	    ;;		(cadr (csvAlist padObject "," ""))
	    ;;		(float xPos) (float yPos) )       ;; fprintf ends

	     ;; Place an instance of the cell
	     (setq figId (dbCreateInst padTopCellId 
				       newCell		
				       instName
				       (list xPos yPos) rot))   ;; setq closed
	     
	     (setq xSurf 0.0)
	     (setq ySurf 0.0)

	     (if (equal rot    "R270") 
		 (progn  
		   (setq yLoff 0)
		   (setq xLoff 55)
		   (setq yPoff -30) 
		   (setq xPoff 45))) ;;if closes

	     (if (equal rot    "R90")  
		 (progn  
		   (setq yLoff 0)
		   (setq xLoff -55)
		   (setq yPoff 30)
		   (setq xPoff -45))) ;;if closes

	     (if (equal rot    "R180") 
		 (progn  
		   (setq yLoff -55)
		   (setq xLoff 0)
		   (setq yPoff -45) 
		   (setq xPoff -15))) ;; if closes

	     (if (equal rot    "R0")   
		 (progn  
		   (setq yLoff 55)
		   (setq xLoff 0)
		   (setq yPoff 45)
		   (setq xPoff 15)))  ;; if closes

	     (setq possibleFlip t)
	     
	     (if (not (member padCellName ignorePadSurfaces)) 
		 (progn
		   (setq numberIsOdd (oddp (evalstring (assoCadr padObject "Nr.")))) ;; only setq closes
		   (if (and possibleFlip numberIsOdd)		       		
		       (progn

                        ;;(if  (equal rot "R0") (setq yPoff yPoff-90) (setq yPoff yPoff+90))

                        ;; (setq surfId
                        ;;       (dbCreateInst padTopCellId
                        ;;                     (if (not (equal (assoCadr padObject "DummyLib") "nil"))
                        ;;                         padSurfaceCellDum2 padSurfaceCell2)
                        ;;                     (strcat "Isurface_" instName )
                        ;;                     (list xPos+xSurf yPos-ySurf) rot))

			(setq surfId
                              (dbCreateInst padTopCellId
                                            (if (not (equal (assoCadr padObject "DummyLib") "nil"))
                                                padSurfaceCellDum2 newCell )
                                            (strcat "Isurface_" instName )
                                            (list xPos+xSurf yPos-ySurf) rot))






                        (leAttachFig
                         (dbCreateLabel padTopCellId "text"
                                        (list (float xPos+xLoff-0.5*padHeight*(caddr (assoc rot rotTransform)) )
                                              (float yPos+yLoff-0.5*padHeight*(cadddr (assoc rot rotTransform))))
                                        (sprintf nil "%d" (setq countIndex (plus countIndex 1)))
                                        "centerCenter"
                                        (cadr (assoc rot rotLabel)) "roman" 0.75*padWidth)
                         surfId)

                        (fprintf posPort "%s,%3.3f,%3.3f,%3.3f,%3.3f,%d\n"
                                 (cadr (csvAlist padObject "," ""))
                                 (float xPos+xLoff-0.5*padHeight*(caddr (assoc rot rotTransform)) )
                                 (float yPos+yLoff-0.5*padHeight*(cadddr (assoc rot rotTransform)))
                                 (float xPos) (float yPos) countIndex)

                        ) ;; internal  progn closes
		     (progn
		       
		       (setq surfId 
			     (dbCreateInst 
			      padTopCellId
			      
			    ;;  (if (not (equal (assoCadr padObject "DummyLib") "nil"))
			    ;;	  padSurfaceCellDum1 padSurfaceCell1)
			      (if (not (equal (assoCadr padObject "DummyLib") "nil"))
			 	  padSurfaceCellDum1 newCell)

			      (strcat "Isurface_" instName)
			      (list xPos+xSurf yPos-ySurf)
			      rot))		   		     
		       
		       (leAttachFig 
			(dbCreateLabel padTopCellId "text" 
				       (list (float xPos-xLoff-0.5*padHeight*(caddr (assoc rot rotTransform)) )
					     (float yPos-yLoff-0.5*padHeight*(cadddr (assoc rot rotTransform))))
				       (sprintf nil "%d" (setq countIndex (plus countIndex 1)))
				       "centerCenter" 
				       (cadr (assoc rot rotLabel)) "roman" 0.75*padWidth)		   
			surfId)

		       
		       (fprintf posPort "%s,%3.3f,%3.3f,%3.3f,%3.3f,%d\n" 
				(cadr (csvAlist padObject "," ""))
				(float xPos-xLoff-0.5*padHeight*(caddr (assoc rot rotTransform)))
				(float yPos-yLoff-0.5*padHeight*(cadddr (assoc rot rotTransform)))
				(float xPos) (float yPos) countIndex)

		       ) ; progn closed
		     )  ; if (and possibleFlip ..) closes here
		   
		   (leAttachFig surfId figId)
		 
		   ) ; upper progn closes
	       ) ; if (not member padCellname ignorePadSurfaces) closes here
	     

	     (if (not (equal (assoCadr padObject "Nr.") "nil"))
		 (progn
		   (setq bBoxPad (daisyTransformCoords rot xPos yPos newCell))
		   (if nil (rodCreateRect ?layer (list "areaid" "substrateCut")  ;; which layer from LSW
					  ?cvId  padTopCellId
					  ?bBox  (list (list 
							(caar  bBoxPad )-0.5*(caadr  bBoxPad )
							(cadar  bBoxPad )-0.5*(cadadr  bBoxPad ))
						       (list
							(caar  bBoxPad )+0.5*(caadr  bBoxPad )
							(cadar  bBoxPad )+0.5*(cadadr  bBoxPad ))
						 )		
					  ?netName (assoCadr padObject "vgnd")  ;; likely to be removed 
					  ) ;; endof-rodCreateRect
		     ) ;;endof-if
		   ) ;; endof- progn 
	       )  ;; endof-if(not...) 
	     
	     
	   ;; Attach the out pin label to the instance
	   (if (not (equal (assoCadr padObject "Nodename") "nil"))
	       (leAttachFig 
		(dbCreateLabel padTopCellId "text" (list xPos yPos) 
			       (assoCadr padObject "Nodename") 
			       (strcat "center" (car (last (assoc rot rotLabel)))) ;; becomes "centreright", endof-strcat
			       (cadr (assoc rot rotLabel)) "roman" 0.75*padWidth ) ;; endof- dbCreateLabel
		figId 
		)
	     ) ;; endof-if(not 
	   
(printf "-------Attaching comment to the instance ... ----------- \n")
	     
	   ;; Attach the comment to the instance

	   (leAttachFig 
	    (dbCreateLabel padTopCellId "text" 
			   (list xPos+(caddr (assoc rot rotLabel))*padWidth*0.25 
				 yPos+(cadddr (assoc rot rotLabel))*padWidth*0.25)
			   (assoCadr padObject "Comment") 
			   (strcat "center" (car (last (assoc rot rotLabel))))
			   (cadr (assoc rot rotLabel)) "stick" 2)
	    figId) ;; endof-leAttachFig 
	   
	   (if (and nil (not (equal (assoCadr padObject "Nr.") "nil")))
	       ;; Place a label at the instance
	       (leAttachFig 
		(dbCreateLabel padTopCellId "text" 
			       (list xPos-0.5*padHeight*(caddr (assoc rot rotTransform)) 
				     yPos-0.5*padHeight*(cadddr (assoc rot rotTransform))) 
			       (assoCadr padObject "Nr.") 
			       "centerCenter" 
			       (cadr (assoc rot rotLabel)) "roman" 0.75*padWidth)
		figId)) ;; if closed
	   
	   
	   ;; Add pins for the terminals of the cell
	   (foreach net (nthcdr 5 padObject) ; leaves the first five -- nthcdr
		    (if (and (not (equal (cadr net) "nil")) 
			     (not (equal (cadr net) nil)))
			(progn
			  ;; Get the pin labels from within the instance.
			  (setq localShapes (setof x figId~>master~>shapes (equal x->theLabel (car net))))
			  ;; (printf "netname ---------> %s \n" (cadr net))
			  (foreach localShape localShapes
				   (setq layerPP localShape~>lpp)
				   ;;(printf "-------------> %L \n" (daisyTransformCoords rot xPos yPos localShape))
				   ;;(printf "-------------> %L \n" 
				   (setq dim  (daisyTransformCoords rot xPos yPos localShape nil))
				   ;;(printf "cpos -------------> %L : %L \n" localShape -> xy 
				   (setq cpos (car dim))
				   (setq dim  (cadr dim))
				   
				   ;; Add the pin
				   (setq pinId (daisyLeCreatePin
						padTopCellId 
						?verbose nil
						?position cpos
						?pinLayer (car layerPP)
						?pinName  (cadr net)
						?dimensions  (list 0.12 0.12)
						?pinDirection  direction
						?labelSize 4
						?labelRot lRot
						?masterId figId)	)		       
				   
				   ;; Create an unique name for the pin
				   (if (equal (assoCadr padObject "Nr.") "nil")
					 (sprintf pinName "%s_NoNum_%d_%s" 
						  (assoCadr padObject "Pinname") noNum  (cadr net)) 
				     (sprintf pinName "Ipad_%s_%s_%s" 
					      (assoCadr padObject "Pinname") (assoCadr padObject "Nr.")  (cadr net)))
				     
				   (while (not (rodIsFigNameUnused pinName padTopCellId))
				     (sprintf pinName "%s_copy" pinName))
				   
				   ;; Assign a rod name for the pin
				   (rodNameShape ?name pinName
						 ?shapeId pinId)				    
				   (setq rodPinNameList 
					 (append (list pinName) rodPinNameList)))
			  ) ;; progn closed
			  (progn
			    ;; Write to the output log if the pin wasn't found within the design
			    (printf " ")
			    ;;(fprintf logPort
			    ;;       "Warning! Pin %s connected to net %s not found in instance %s.\n"
			    ;;     (car net) (cadr net) instName) 
			   
			    )) ;; endprogn and endif 
		    ) ;; (foreach net) closes
	   
	   (setq oldCell newCell)
	   
	   ;; Check if additional filler cell needs to be added or not (specified in spread sheet):
	   ;; ========
	   (if (equal (assoCadr padObject "Filler") "t")
	       (progn		   
		 ;; Place an instance of the cell
		 ;; First create new coordinates for the filler cell:
		 (setq newPos (getInstanceSpace (list xPos yPos) 
						 rot masterFillerCell oldCell)) ;; endof-setq
		 
		 (setq xPos (car newPos))
		 (setq yPos (cadr newPos))	   	     		 
		 
		 (dbCreateInst padTopCellId 
			       masterFillerCell
			       (strcat "fill_" instName)
			       newPos  rot)
		 
		 ;; Update the new position for next pad cell.		 
		 (setq oldCell masterFillerCell)
		 ) ;; endof-progn
	     ) ;; endof-if 
	   ) ;; endof-progn  
      ) ;; if (equal(assoCadr padObject "Pinname") "CORNER")  closes
) ;; (foreach padObject padList closes

;; Close the output log files
(close posPort)
(close logPort) 
(close padsPort)

;; Create the instance rectangle in the top left-hand corner
(foreach purpose (list "drawing" "boundary")
	 (rodCreateRect ?layer (list "prBoundary" purpose)
			?cvId  padTopCellId
			?bBox  (list originP 
				     upperXY))) ;;foreach closed 

;; Move Origin -- 
(leMoveCellViewOrigin padTopCellId originP)
(rodCreateRect ?layer "instance"
	       ?cvId  padTopCellId
	       ?bBox  (list (list 0 0) (list padHeight padHeight)))

(hiZoomAbsoluteScale winId 0.9)

(dbCopyCellView 
 padTopCellId
 workLibName
 cellView
 (strcat viewType rev)
 nil nil t)

;; ==
