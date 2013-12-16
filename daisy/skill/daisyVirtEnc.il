
;; This could be picked up from technology devices section instead.
;; Global via rules.
(setq listOfVias  
      (list 
       (list "M2_M1" (list "M2" "M1" 0.13 0.3) )
       (list "M3_M2" (list "M3" "M2" 0.13 0.3))
       (list "M4_M3" (list "M4" "M3" 0.13 0.3))
       (list "M5_M4" (list "M5" "M4" 0.13 0.3))
       (list "M6_M5" (list "M6" "M5" 0.36 0.9))
       
       (list "M2_M1c" (list "M2" "M1" 0.13 0.3))
       (list "M3_M2c" (list "M3" "M2" 0.13 0.3))
       (list "M4_M3c" (list "M4" "M3" 0.13 0.3))
       (list "M5_M4c" (list "M5" "M4" 0.13 0.3))
       (list "M6_M5c" (list "M6" "M5" 0.36 0.9))
       
       (list "DFM_M2_M1" (list "M2" "M1" 0.13 0.3))
       (list "DFM_M3_M2" (list "M3" "M2" 0.13 0.3))
       (list "DFM_M4_M3" (list "M4" "M3" 0.13 0.3))
       (list "DFM_M5_M4" (list "M5" "M4" 0.13 0.3))       
       ))

(defun daisyCalcNoVias (via)
  ;; Now a quite dumb function, but encapsulated 
  ;; anyway for future use.
  (let ((viaRowCol (list nil nil)))    
    (setq viaRowCol
	  (list 
	   (if (car (setof x via->prop (equal x->name "row")))->value
	     (car (setof x via->prop (equal x->name "row")))->value
	     1)
	   (if (car (setof x via->prop (equal x->name "column")))->value
	     (car (setof x via->prop (equal x->name "column")))->value
	     1)))
    (if (member via->orient 
		(list "R90" "MXR90" "R270" "MYR90"))	
	(setq viaRowCol (reverse viaRowCol)))
    viaRowCol))

(defun daisyDrawViaWire (via @optional (cv nil) )
  ;; Draws vias on top of all the vias.
  (let (
	(bBox via->bBox)
	(xCenter 0.0) 
	(yCenter 0.0))
    (if (not cv) (setq cv (hiGetCurrentWindow)->cellView) )

    (setq rows    (car (daisyCalcNoVias via)))
    (setq cols    (cadr (daisyCalcNoVias via)))
    (setq metal1  (nth 0 (cadr  (assoc via->cellName listOfVias)) ))
    (setq metal2  (nth 1 (cadr  (assoc via->cellName listOfVias)) ))

    ;; viaSize should normally not be changed in this process
    (setq viaSize (nth 2 (cadr  (assoc via->cellName listOfVias)) ))

    ;; This could have been changed by the user, but we could then 
    ;; pick up property yPitch/xPitch instead. (But we need to compensate
    ;; path formula below for both x and y.
    (setq viaSpacing (nth 3 (cadr  (assoc via->cellName listOfVias)) ))
    
    ;; Calculate the required path widths
    (setq horPathWidth viaSpacing*(rows-1)+viaSize)
    (setq verPathWidth viaSpacing*(cols-1)+viaSize)
    
    ;; Find the center point of the contact (now assume symmetry...)
    (setq xCenter 0.5 * ( (caar bBox)  + (caadr bBox  )))
    (setq yCenter 0.5 * ( (cadar bBox) + (cadadr bBox )))
        
    ;; Generate two metal strips for each via, make sure the via 
    ;; holes are covered.
    (foreach metal (list metal1 metal2)
	     (foreach direct (list 0.0) ;; Do we need in both directions...
		      (setq path
			    (dbCreatePath 
			     cv metal
			     (list
			      (list 
			       xCenter-direct*verPathWidth/2.0
			       yCenter-(1-direct)*horPathWidth/2.0
			       )
			      (list 
			       xCenter+direct*verPathWidth/2.0
			       yCenter+(1-direct)*horPathWidth/2.0
			       ))			     
			     direct*horPathWidth+(1-direct)*verPathWidth) )
		      ;; Tag the wires with property (which user can alter if
		      ;; some strips should be kept.
		      (dbCreateProp path "metalStripForEncounter" "boolean" t)
		      ;; Attach to the via for easy moving
		      (leAttachFig path via)
		      ))))

(defun daisyStripEncounterStrips (@optional (cv nil))
  ;; Use to remove all the metal strips
  (let (( shapeWithTags (list) ))
    (if (not cv) (setq cv (hiGetCurrentWindow)->cellView))
    (setq shapeWithTags	  
	  (setof x cv->shapes 
		 (member "metalStripForEncounter"
			 x->prop~>name)) )
    (foreach x shapeWithTags
	     (if (equal x->metalStripForEncounter "TRUE")
		 (dbDeleteObject x)))))

(defun daisyCreateEncounterStrips (@optional (cv nil))
  ;; Use to create all the metal strips
  (if (not cv) (setq cv (hiGetCurrentWindow)->cellView))
  (foreach localVia 
	   (setof x cv->instances 
		  (assoc x->cellName listOfVias))
	   (daisyDrawViaWire localVia cv)))
