;; libName = "CORE65GPSVT"
;; cellName = "HS65_GSS_DFPQNX18"
;; (daisyCreateStdCellMask "CORE65GPSVT" "HS65_GSS_DFPQNX18")
;; tfName = "cmos065"

(defun daisyCreateStdCellMask (libName cellName @optional (tfName "cmos065"))

  (let ( (libName_mask "") 
	 (libId nil)
	 (llId nil)
	 (nId nil)
	 (pId nil)
	 (tdId nil) 
	 (cellId nil)
	 (cellId2 nil) 
	 (metals nil)
	 (termList nil)
	 (pins nil) )
    
  libName_mask = (strcat libName "_mask")
  cellName_mask = (strcat cellName "_")

  (if (not (ddGetObj libName_mask))
      (setq libId  (ddCreateLib libName_mask (strcat (getShellEnvVar "PDK_HOME") "/oa/" libName_mask)))
    (setq libId (ddGetObj libName_mask)))

  tfId = (techGetTechFile (ddGetObj tfName))
  (techSetTechLibName libId tfName)
  
  cellId = (dbOpenCellViewByType libName cellName "layout")
  metals = (setof x cellId->shapes (and (equal x->layerName "M1") (not (equal x->objType "label"))))
  pins   = (setof x cellId->shapes (equal x->purpose "pin"))
  
  cellId2 = (dbOpenCellViewByType libName_mask cellName_mask "layout" "maskLayout" "w")
  (dbCreateInst cellId2 cellId "|I_cell" (list 0.0 0.0) "R0")
  (dbSave cellId2)
  
  (foreach object metals
	   (if object->points
	       (progn
		 nId = (dbMakeNet cellId2 object->net->name)
		 pId = (dbCreatePolygon cellId2 "M1" object->points)
		 (if nId (dbAddFigToNet pId nId)))
	     (progn
	       nId = (dbMakeNet cellId2 object->net->name)	    
	       pId = (dbCreateRect cellId2 "M1" object->bBox)
	       (if nId (dbAddFigToNet pId nId)))
	     ))
  
  (foreach object pins
	   (if (equal object->objType "label")
	       (progn 
		 llId = (dbCreateLabel 
			 cellId2 
			 (list (if (member object->theLabel (list "vdd" "gnd")) "M1" "M2")  "pin") object->xy
			 object->theLabel "centerCenter" "R0" "swedish" 0.1 )
		 netId  = (dbMakeNet cellId2 object->theLabel)
		 (dbAddFigToNet llId netId)
		 (if (not (member object->theLabel (list "vdd" "gnd")))
		     (dbCreateVia cellId2 techFindViaDefByName(tfId "M2X_M1") object->xy "R0")
		   
		   ))
	     
	     (progn 	     
	       llId = (dbCreateRect cellId2 (list "M1" "pin") object->bBox)
	       (if object->net->name
		   (progn
		     nId =  (dbMakeNet cellId2 object->net->name)		   
		     (if nId (dbCreatePin nId llId))
		     (if nId (dbAddFigToNet llId nId)))))))

(dbSave cellId2)
(dbClose cellId2)

(dbClose cellId)


cellId = (dbOpenCellViewByType libName cellName "symbol")
(dbCopyCellView cellId libName_mask cellName_mask "symbol")
cellId2 = (dbOpenCellViewByType libName_mask cellName "symbol")
(dbSave cellId2) (dbClose cellId2) 

(printf "%s %s \n" cellName libName_mask)

cellId2 = (dbOpenCellViewByType libName_mask cellName_mask "schematic" "schematic" "w")

instId  = (dbCreateInst cellId2 cellId "I_cell" (list 0.0 0.0) "R0")

(setq ipin (dbOpenCellViewByType "basic" "ipin" "symbol"))
(setq opin (dbOpenCellViewByType "basic" "opin" "symbol"))

m = 1
termList = ()
(foreach object cellId->terminals
	 thisNet = (dbMakeNet cellId2 object->name)
	 (setq thisPin (dbCreateInst 
			cellId2 
			(if (equal object->direction "input") ipin opin) object->name (list 0 m) "R0"))
	 (dbCreateTerm thisNet object->name object->direction)	 
	 (dbCreatePin thisNet thisPin object->name)
	 m = m + 1
	 (setq termList (append termList (list (list object->name object->name))))
	 )

(dbCheck cellId2)

(daisyAddSupplyNames termList (list instId) cellId2)
(dbCheck cellId2)
(dbSave cellId2)
(dbClose cellId2) 
)
)
