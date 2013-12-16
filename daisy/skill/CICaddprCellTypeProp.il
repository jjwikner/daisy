
/*******************************************************************************
 *                                                                             *
 *       +---_-----------+   Copyright (c) 2004                                *
 *       | c a d e n c e |   Cadence Design Systems Ltd.                       *
 *       +---------------+                                                     *
 *                           All Rights Reserved                               *
 *                                                                             *
 ******************************************************************************/
 
 
/*******************************************************************************
 *
 * Filename: 
 * Version: %W% %G%
 * Author: Danny Nelhams
 * Changes:
 * 05oct07 mcordrey Changed from abstact view to layout view
 *
 ******************************************************************************/

(defun 
  CICaddprCellTypeProp ( @optional
			 (cv geGetEditCellView() )
			 )
  (printf "WTF ? \n")
  (let (abstracts)
    (setq abstracts (setof cell 
			   cv~>instHeaders 
			   (and (or 
				 (equal cell~>viewName "layout" ) 
				 (equal cell~>viewName "abstract" ) )
				(not (equal cell->libName "tsmcN80")))))
    (printf "%L \n" abstracts)
    ;; try to open each abstract in read only mode..
    (foreach cell abstracts
	     (printf "Checking cell %L \n" cell~>cellName)
	     (setq cell  
		   (dbOpenCellViewByType
		    cell~>libName
		    cell~>cellName
		    cell~>viewName
		    ""
		    "a"
		    ))
	     ;; dbOpenCellViewByType
	     
	     ;; look for prCellType prop, if not found add it.
	     (when cell
	       (unless (dbGetPropByName cell "prCellType") 		 
		 (when (dbReopen cell "a") 
		   (printf "** Adding prop prCellType to cell %L**\n" cell~>cellName)
		   (dbCreateProp cell "prCellType" 'string "macro")
		   (dbSave cell)
		   ); when
		 ); unless
	       ); when	    
	     (dbClose cell)
	     ); foreach
    t
    ); let
  ); procedure


(defun  CICaddprCellTypePropLite ( @optional ( propList (list 
							 (list "prCellType"  "macro" )
							 (list "symmetry" "xyr90")
							 (list "allowedOrient" "xyr90")))					     
					     (cell hiGetCurrentWindow()->cellView) 
					     )

  (let ( (propName "") (propVal "") )

    (printf "Checking cell %L \n" cell~>cellName)
    
    ;; look for prCellType prop, if not found add it.
    
    (when cell  
      (foreach propPair propList
	       (setq propName (car propPair))
	       (setq propVal (cadr propPair))	       
	       (unless ( dbGetPropByName cell propName )
		 (when (dbReopen cell "a") 
		   (printf "** Adding prop %s to cell %L**\n" cell~>cellName propName)
		   (dbCreateProp cell propName 'string propVal)
		   (dbSave cell)
		   )))
      (dbClose cell)
      )))


(defun CICaddprAllCells ( libName )
  (let ( ( libId nil ) 
	 ( cellList nil ) ) 
    (setq libId (car (setof x (ddGetLibList) (equal x->name libName))))
    (setq cellList libId->cells)
    
    (foreach cell cellList
	     (CICaddprCellTypePropLite  (list (list "prCellType"  "macro" )
					      (list "symmetry" "xyr90")
					      (list "allowedOrient" "xyr90"))
					(dbOpenCellViewByType 
					 libName
					 cell->name
					 "abstract" "maskLayout" "r")))
    ))

(defun daisyAddDistProp ( libName viewName )
  (let ( ( libId nil ) 
	 ( cellList nil ) 
	 ( cellId nil ) 
	 ( distList nil )
	 ) 
    (setq libId (car (setof x (ddGetLibList) (equal x->name libName))))
    (setq cellList libId->cells)
    (printf "%L" cellList)
    (foreach cell  cellList	     
	     (setq cellId 
		   (dbOpenCellViewByType 
		    libName
		    cell->name
		    viewName
		    "maskLayout" "r"))
	     
	     (setq distList (setof x cellId->instances (member x->cellName (list "FILL1" "FILL2" "FILL4" 
									     "FILL8" "FILL16" "FILL32" "FILL64"
									     "DCAP" "DCAP2" "DCAP4" 
									     "DCAP8" "DCAP16" "DCAP32" "DCAP64"
									     "FILLHVT1" "FILLHVT2" "FILLHVT4" 
									     "FILLHVT8" "FILLHVT16" "FILLHVT32" 
									     "FILLHVT64"									     
									     "DCAPHVT" "DCAPHVT2" "DCAPHVT4" 
									     "DCAPHVT8" "DCAPHVT16" "DCAPHVT32"
									     "DCAPHVT64" ))))
	     
	     (if distList
		 (progn
		   (dbReopen cellId "a")
		   (print "enchillada \n" )
		   (foreach inst distList
			    (dbDeletePropByName inst "source")
			    (dbCreateProp inst "source" 'string "DIST"))
		   (dbSave cellId) ) ) )
    ))

;; CICaddprAllCells("work_jawi_defLef")~>views~>name

