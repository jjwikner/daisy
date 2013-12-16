;; loadi("/home/tde/emilh/SiCon/contactExpand.il")
;; loadi("/home/tde/emilh/SiCon/jjmods/leMods.il")
;; techGetSpacingRules(tfId)

(setq cv (dbOpenCellViewByType 
	  "SiCon" 
	  "sample1" 
	  "layout" "maskLayout" "r"))

(EHresizeContacts cv)

(defun EHresizeContacts (cv)
  ;; Set the contact reference list
  ;; associatingt the contact name with the via layer name
  ;; The order is list(<contact name> <via layer name>)
  (setq viaList (list
		 (list "VIA1_C" "VIA1") 
		 (list "VIA2_C" "VIA2") 
		 (list "VIA3_C" "VIA3")))
  
  ;; Get all contacts in the design
  (setq contacts 
	(setof x 
	       cv~>instances 
	       (exists y viaList x~>cellName==car(y))))
  
  ;; Get techfile
  (setq tfId (techGetTechFile cv))
  
  ;; Go through all contacts in the design
  (foreach contact contacts
	   
	   ;; Get connecting paths for the current VIA
	   (setq upperLayer caddar(setof(x techGetViaLayers(tfId) cadr(x) == cadr(assoc(contact~>cellName viaList)))))
	   (setq lowerLayer caar(setof(x techGetViaLayers(tfId) cadr(x) == cadr(assoc(contact~>cellName viaList)))))
	   (setq upperPath  (dbGetTrueOverlaps cv contact~>bBox upperLayer))
	   (setq lowerPath  (dbGetTrueOverlaps cv contact~>bBox lowerLayer))
	   
	   (if (or (upperPath == nil) 
		   (lowerPath == nil))
	       (progn
		 (printf "Warning! unconected VIA of type %s\n"
			 contact~>cellName))
	     (progn
	       (printf "Investigating VIA of type %s with bBox = %L "
		       contact~>cellName contact~>bBox)
	       
	       ;; Find overlap between the paths conected to the via
	       (setq overlap (dbLayerAnd
			      cv
			      upperLayer
			      upperPath
			      lowerPath))
	       (setq newContactBox (car overlap~>bBox))
	       (dbDeleteObject (car overlap))
	       
	       ;; Create new contact
	       (setq newContact (EHcreateContact contact newContactBox cv))
	       
	       ;; Check if the old contact was larger tha the new one.
	       ;; If the original contact is larger than teh new one in any direction, the original one is keept.
	       (if (or (greaterp (abs xCoord(car(contact~>bBox))-xCoord(cadr(contact~>bBox)))  
				 (abs xCoord(car(newContact~>bBox))-xCoord(cadr(newContact~>bBox))))
		       (greaterp (abs yCoord(car(contact~>bBox))-yCoord(cadr(contact~>bBox))) 
				 (abs yCoord(car(newContact~>bBox))-yCoord(cadr(newContact~>bBox)))))
		   (progn
		     (dbDeleteObject newContact)
		     (printf "- original VIA kept.\n"))
		 
		 (progn
		   (dbDeleteObject contact)
		   (printf "- original VIA replaced.\n")))))))


(defun EHcreateContact (contact box cv)
  ;; Get techfile
  (setq tfId (techGetTechFile cv))
  ;; Get VIA name
  (setq via cadr(assoc(contact~>cellName viaList)))
  
  ;; Get VIA connecting layers
  upperLayer = caddar(setof(x techGetViaLayers(tfId) cadr(x) == via))
  lowerLayer = caar(setof(x techGetViaLayers(tfId) cadr(x) == via))
  
  ;; Get VIA spacing
  minWidth = (techGetSpacingRule tfId "minWidth" via)
  minSpacing = (techGetSpacingRule tfId "minSpacing" via)
  enclosure = (max (techGetSpacingRule tfId "minEnclosure" upperLayer via) 
		   (techGetSpacingRule tfId "minEnclosure" lowerLayer via))
  
  ;; Get maximum area for VIA
  xWidth = abs(xCoord(car(box))-xCoord(cadr(box)))
  yWidth = abs(yCoord(car(box))-yCoord(cadr(box)))
  
  ;; Compute maximum number of cols and rows for the VIA
  cols = max(floor((xWidth+minSpacing-2.0*enclosure)/(minSpacing+minWidth)) 1)
  rows = max(floor((yWidth+minSpacing-2.0*enclosure)/(minSpacing+minWidth)) 1)
  
  ;; Compute VIA center
  contactCenter = (list (xCoord(cadr(newContactBox))-xCoord(car(newContactBox)))/2+xCoord(car(newContactBox))
			(yCoord(cadr(newContactBox))-yCoord(car(newContactBox)))/2+yCoord(car(newContactBox)))
  
  ;; Create VIA
  (leCreateContact
   cv
   contact~>cellName
   contactCenter
   "R0" 
   minWidth
   minWidth
   rows
   cols 
   minSpacing+minWidth
   minSpacing+minWidth
   "center" 
   "center"))
