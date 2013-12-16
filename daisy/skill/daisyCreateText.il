printf(" ... loading daisyCreateText.il ... \n")

(defun daisyCreateLabel (textString cvId 
				    @key 
				    (magnification 1.0)
				    (maxWidth      200.0)
				    (space         15.12)
				    (startPoint    (list 0.0 0.0)))
  
  (let (( alphaId nil ) 
	( letterId nil )
	( maxYbox 0.0)
	( alphaLib "daisyAlpha" ))
    
    xStart = (car  startPoint)
    yStart = (cadr startPoint)
    
    x = xStart
    y = yStart
    
    (for k 1 (strlen textString)     
	 (if (not (equal (substring textString k 1) " " ))
	     (progn
	       (setq letterId
		     (dbCreateInst 
		      cvId  
		      (setq alphaId 
			    (dbOpenCellViewByType 
			     "daisyAlpha" 
			     (strcat 
			      "daisyAlpha" 
			      (upperCase (substring textString k 1)))
			     "layout" "" "r")) ""
			     
			     ;;(sprintf nil "|ItextString%d" k)
			     (list x y)
			     "R0")
		     )
	       letterId->magnification=magnification
	       (if (greaterp (cadadr alphaId->bBox)  maxYbox)
		   (setq maxYbox (cadadr alphaId->bBox)))
	       (setq x (plus x (caadr alphaId->bBox)))
	       )
	   (setq x (plus x space*magnification)))
	 
	 (if (greaterp x (xStart + maxWidth))
	     (progn 
	       (setq x xStart)
	       (setq y y-magnification*space-maxYbox))
	       (setq maxYbox 0.0)))
	 )
    )
  
(if nil
(daisyCreateLabel 
 "Daisy SemiConductor 80nm AFE IP OCT 2008" 
 (dbOpenCellViewByType "work_jawiTest" "test" "layout" "" "a")
 ?magnification 0.2 
 ?maxWidth 400)
)

(daisyCreateLabel 
 "jawi 081023"
 (dbOpenCellViewByType "work_jawiTest" "test" "layout" "" "a")
 ?startPoint (list 100 100)
 ?magnification 1 
 ?maxWidth 400)
