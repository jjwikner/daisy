;; ======================================
(printf  " ... loading %s ... \n" "daisyLayoutMods")
;; ======================================

(defun displayDrcErrors ()
  (let ((cw (getq (hiGetCurrentWindow) cellView)))
    (printf "%L \n" cw->??)
    (sh (sprintf nil "perl %s/bin/jjdrc.pl %s %s" 
		 (getShellEnvVar "HOME")
		 cw -> libName
		 cw -> cellName))
    
    (loadi (sprintf nil "/%s/WA/v/%s/%s/drc/drcError.db.il"
		    (getShellEnvVar "HOME")
		    cw->libName
		    cw->cellName))) 
  LISTOFDRCERRORS)

(defun zoomTo (x y @optional (scale 1)) 
  (let ((cw (hiGetCurrentWindow)))
    (hiZoomIn cw (list (list x-scale*10 y-scale*10) (list x+scale*10 y+scale*10)))))

(defun zT (x y @optional (scale 1)) 
  (zoomTo x y scale))

(defun getXY ()
  (let ((cw (hiGetCurrentWindow)))
    (printf "(list %5.3f %5.3f)" 
	    (float 0.001*(car (hiGetDbuPoint cw))) 
	    (float 0.001*(cadr (hiGetDbuPoint cw))) )))

(defun daisyLeCreateLabel (cellId layerList posList name 
				@optional (height 0.2) (masterId nil))
  (let ( (labelId nil) )
    (foreach layer (if (not (listp layerList)) 
		       (list layerList) layerList)
	     (setq labelId 
		   (dbCreateLabel cellId  
				  (list layer "pin")
				  posList
				  name
				  "centerCenter"  
				  "R0" "stick" height))
	     (if masterId (leAttachFig labelId masterId)))
    labelId))

(defun  daisyLeCreateShieldedBus (masterPathList deltaList layerList wireWidth cv)
  (let 
      ( subPathList
	rodObj
	tfId
	shieldWidthLR
	shieldSpace
	minpt
	maxpt
	shieldWidthTB
	)
    
    ;; Get min size wires for shield from techfile
    (setq tfId (techGetTechFile cv))
    (setq shieldWidthLR  (techGetSpacingRule tfId "minWidth"   (nth 1 layerList)))
    (setq shieldSpace    (techGetSpacingRule tfId "minSpacing" (nth 1 layerList)))
    (setq shieldWidthLR 1)
    (setq shieldSpace   1)
    
    ;; Find width of top and bottom shields
    (setq minpt  (car deltaList))
    (foreach x deltaList 
	     (setq minpt (min x minpt)))
    
    (setq maxpt  (car deltaList))
    (foreach x deltaList 
	     (setq maxpt (max x maxpt)))
    
    (setq shieldWidthTB (abs maxpt-minpt)+wireWidth+2.0*(shieldWidthLR+shieldSpace))
        
    (setq   subPathList nil)
    ;; Create bottom shield
    (setq subPathList (append1 subPathList
			       (list ?layer    (nth 0 layerList)
				     ?sep    ((abs maxpt)-(abs minpt))/2.0
				     ?width    shieldWidthTB)))
    ;; Create top shield
    (setq subPathList  (append1 subPathList
			   (list ?layer    (nth 2 layerList)
				 ?sep    ((abs maxpt)-(abs minpt))/2.0
				 ?width    shieldWidthTB)))
    
    ;; Create side shield #1
    subPathList = (append1 subPathList
			   (list ?layer    (nth 1 layerList)
				 ?sep    minpt-shieldWidthLR/2.0-wireWidth/2.0-shieldSpace
				 ?width    shieldWidthLR))    

    ;; Create side shield #2
    subPathList = (append1 subPathList
			   (list ?layer    (nth 1 layerList)
				 ?sep    maxpt+shieldWidthLR/2.0+wireWidth/2.0+shieldSpace
				 ?width    shieldWidthLR))
    
    ;; Create subpath list
    
    (for n 0 (length deltaList)
	 subPathList = (append1 subPathList
				(list ?layer           (nth 1 layerList)
				      ?sep             (nth n deltaList))))

    (printf "%L\n" (nth 1 layerList))
    (printf "%L\n" subPathList)
    ;; Create rod path with subpaths
    (setq rodObj 
	  (rodCreatePath  
	   ?layer           (nth 1 layerList)
	   ?pts             masterPathList
	   ?width           wireWidth
	   ?justification   "center"
	   ?cvId            cv
	   ?offsetSubPath   subPathList))
    rodObj))
  

(defun daisyLeCreateBusRod (cellId masterPathList nrPaths deltaWire layer wireWidth @optional (objId nil))
  (let ( (subPathList nil) 
	 (rodObj nil)
	 ) 
    
    ;; Create subpath list
    (setq subPathList nil)
    (for n 0 (difference nrPaths 1)	   
	 (setq subPathList (append1 subPathList
				    (list 
				     ?width           wireWidth
				     ?layer           layer
				     ?sep             (times n deltaWire)))))
    ;; Create rod path with subpaths
    (setq rodObj (rodCreatePath
		  ?layer           layer
		  ?pts             masterPathList
		  ?width           wireWidth
		  ?justification   "center"
		  ?cvId            cellId
		  ?offsetSubPath   subPathList))
    ;; (if objId (leAttachFig objId rodObj))
    rodObj))

(defun daisyLefindPaths (masterPath nrPaths @optional (deltaWire 0.9)) 
  (let (( returnPaths nil) 
	(index 0)
	(routePath nil)
	(routeDir nil)
	(prevDir nil)
	(dir ""))
  (setq returnPaths (list masterPath))
  
  (setq routePath masterPath)
  (setq routeDir nil)
  (while routePath
    (if (not (cdr routePath)) ;; Last element
	(setq dir "last")
      (if (equal (caar routePath)  (caadr routePath))
	  (setq dir "vert")
	(setq dir "hor")))
    (setq routeDir (append routeDir (list dir)))
    (setq routePath (cdr routePath)))

  (setq routePath masterPath)
  (setq index 0)
  (printf "%L\n ================" routeDir)
  
  (while (lessp index nrPaths)
    (setq newPathList nil)
    (setq newIndex 0)
    (setq prevDir nil)
    (foreach dir routeDir 	   
	     (if (equal dir "hor")
		 (setq newPathList (append (list (list (car (nth newIndex routePath))+(if prevDir deltaWire*index 0) 
						       (cadr (nth newIndex routePath))-deltaWire*index)) newPathList)))
	     (if (equal dir "vert")
		 (setq newPathList (append (list (list (car (nth newIndex routePath))+deltaWire*index  
						       (if prevDir -deltaWire*index 0)+(cadr (nth newIndex routePath)))) newPathList)))
	     (if (equal dir "last")
		 (setq newPathList (append (list (list (car (nth newIndex routePath))+deltaWire*index  
						       (cadr (nth newIndex routePath)))) newPathList)))
	     (setq newIndex (newIndex+1))
	     (setq prevDir dir))
    (setq returnPaths (append (list newPathList) returnPaths))
    (setq index (index+1)))
  returnPaths))

(defun alignAverageXY (xyDir)
  ;; Takes a selected set of graphical objects and aligns
  ;; them in either X or Y direction to the average x/y value.  
  (let ( (sumX 0) (sumY 0) (lenN 0) (avgX 0) (avgY 0) (sumX_p 0) 
	 (dbType nil) (dbPointer nil) (sumY_p 0) )    

    (foreach dbObject (geGetObjectSelectedSet) 
	     (setq dbPointer (car dbObject))
	     (setq dbType dbPointer->objType)
	     (if (member dbType  (list "inst" "mosaicInst" "mosaic")) 
		 (progn 
		   (setq lenN lenN+1)
		   (setq sumX (sumX + (car dbPointer -> xy)))
		   (setq sumY (sumY +  (cadr dbPointer -> xy)))))	     
	     (if (equal dbType "path") 
		 (progn
		   (setq sumX_p 0)
		   (setq sumY_p 0)
		   (setq lenN_p dbPointer -> nPoints)
		   (setq lenN   (lenN + 1))
		   (foreach dbPoints dbPointer->points 
			    (setq sumX_p sumX_p +  (car dbPoints))
			    (setq sumY_p sumY_p +  (cadr dbPoints)))
		   (setq sumX_p  sumX_p / lenN_p)
		   (setq sumY_p  sumY_p / lenN_p)
		   (setq sumX    sumX + sumX_p)
		   (setq sumY    sumY + sumY_p) ) ) ) 
    (if (not (equal lenN 0)) 
	(progn
	  (setq avgX sumX / lenN)
	  (setq avgY sumY / lenN)
	  (foreach dbObject (geGetObjectSelectedSet) 
		   (setq dbPointer (car dbObject))
		   (setq dbType dbPointer ->  objType)
		   (if (member dbType (list "inst" "mosaicInst" "mosaic")) 
		       (progn 
			 (setq x (car dbPointer -> xy))
			 (setq y (cadr dbPointer -> xy))
			 ;; NOTICE!!! THESE VALUES ARE NOT ALIGNED TO GRID!!!
			 (if (equal xyDir "x") dbPointer ->  xy =  (list avgX y))
			 (if (equal xyDir "y") dbPointer ->  xy =  (list x avgY)) ))
		   
		   (if (equal dbType "path") 
		       (progn 
			 (setq sumX_p 0)
			 (setq sumY_p 0)
			 (setq lenN_p dbPointer -> nPoints)
			 (foreach dbPoints dbPointer->points 
				  (setq sumX_p sumX_p + (car dbPoints))
				  (setq sumY_p sumY_p + (cadr dbPoints)))
			 (setq sumX_p sumX_p / lenN_p)
			 (setq sumY_p sumY_p / lenN_p)
			 (setq xList_p  (list))
			 (setq yList_p  (list))	 
			 (foreach  dbPoints dbPointer->points 
				   (setq  xList_p 
					  (append xList_p 
						  (list (list (car dbPoints)-sumX_p+avgX (cadr dbPoints)))))
				   (setq yList_p 
					 (append yList_p 
						 (list (list (car dbPoints) (cadr dbPoints)-sumY_p+avgY)))))
			 ;; NOTICE!!! THE VALUES ARE NOT ALIGNED TO GRID!!!     
			 (if (equal xyDir "x") dbPointer -> points = xList_p)
			 (if (equal xyDir "y") dbPointer -> points = yList_p)))
		   )))
    ;; Align to grid
    (daisyGridObjectXY "xy" 1)))

(defun mirrorXY (xyDir)
  ;; Takes a selected set of graphical objects and
  ;; reflects them in X or Y direction.
  ;;   xyDir               Direction "x" or "y"
  ;; Returns t  
  (let ( (dbPointer nil) (dbType nil) 
	 (orientList 
	  (list (list "R0"    "MY"    "MX"   ) 
		(list "R90"   "MXR90" "MYR90")
		(list "R180"  "MX"    "MY"   )
		(list "R270"  "MYR90" "MXR90")
		(list "MY"    "R0"    "R180" )
		(list "MYR90" "R270"  "R90"  )
		(list "MX"    "R180"  "R0"   )
		(list "MXR90" "R90"   "R270" ))))
    
    (foreach dbObject (geGetObjectSelectedSet) 
	     (setq dbPointer (car dbObject))
	     (setq dbType dbPointer->objType)	     
	     (if (member dbType (list "inst" "mosaicInst" "mosaic")) 
		 (progn 
		   (setq dbOrient dbPointer -> orient)
		   dbPointer->orient = (if (equal xyDir "x") 
					   (cadr (assoc dbOrient orientList)) 
					 (caddr (assoc dbOrient orientList))))))))

(defun daisyGridObjectXY (xyDir magnifier @optional (dbObjs nil))
  ;;  xSnap =  magnifier * envGetVal("layout" "xSnapSpacing")
  ;;  ySnap =  magnifier * envGetVal("layout" "ySnapSpacing")  
  (let ( (cw (hiGetCurrentWindow))
	 (xSnap 0) (ySnap 0) (xStart 0) (yStart 0)
	 (xstartG 0) (ystartG 0)
	 (xstopG 0) (xstop 0) (ystopG 0)
	 (xmidG 0) (ymidG 0) (hWidth 0) (hHeight 0)
	 (len_N 0) (dbPointer nil) (dbType nil)
	 (x 0) (y 0) (l_objects nil)
	 (xList_p  nil) (yList_p nil) (xyList_p nil))    
    (setq xSnap magnifier * (cw->xSnapSpacing))
    (setq ySnap magnifier * (cw->ySnapSpacing))
    (setq len_N (length (geGetObjectSelectedSet)))    
    (printf "Aligning %d object%s to fundamental grid times %4.3f.\n" 
	    len_N (if (not (equal len_N 1)) "s" "") (float magnifier))
    (if (not dbObjs) 
	(setq l_objects (geGetObjectSelectedSet))
      (setq l_objects dbObjs))    
    (foreach  dbObject l_objects
	      (setq xList_p nil)
	      (setq yList_p nil)
	      (setq xyList_p nil)
	      (setq dbPointer (car dbObject))
	      (setq dbType dbPointer->objType)
	      (if (member dbType (list "inst" "mosaicInst" "mosaic" "label")) 
		  (progn 
		    (setq x (car dbPointer->xy))
		    (setq y (cadr dbPointer->xy))      
		    (printf "Old xy positions: (%f %f) [unit]. \n" x y)
		    (if (equal xyDir "x") dbPointer->xy = (list xSnap*round(x/xSnap) y))
		    (if (equal xyDir "y") dbPointer->xy = (list x ySnap*round(y/ySnap)))
		    (if (equal xyDir "xy") 
			dbPointer->xy = (list xSnap*round(x/xSnap) ySnap*round(y/ySnap)))))
	      (if (or (equal dbType "path") (equal dbType "line") (equal dbType "polygon")) 
		  (progn
		    (foreach dbPoints dbPointer->points      
			     (setq x (car dbPoints))
			     (setq y (cadr dbPoints))
			     (setq xList_p  (append xList_p  (list (list xSnap*(round x/xSnap) y))))
			     (setq yList_p  (append yList_p  (list (list x ySnap*(round y/ySnap)))))
			     (setq xyList_p 
				   (append xyList_p 
					   (list (list xSnap*(round x/xSnap) ySnap*(round y/ySnap))))) t)
		    (if (equal xyDir  "x") dbPointer->points = xList_p)
		    (if (equal xyDir  "y") dbPointer->points = yList_p)
		    (if (equal xyDir "xy") dbPointer->points = xyList_p)))
	      (if (equal dbType "rect") 
		  (progn 
		    (setq xstart (caar dbPointer->bBox))
		    (setq ystart (cadar dbPointer->bBox))
		    (setq xstop  (caadr dbPointer->bBox))
		    (setq ystop  (cadadr dbPointer->bBox))		
		    ;; Align edges
		    (setq xstartG  xSnap*round(xstart/xSnap))
		    (setq ystartG  ySnap*round(ystart/ySnap))
		    (setq xstopG   xSnap*round(xstop/xSnap))
		    (setq ystopG   ySnap*round(ystop/ySnap))))
	      (if (dbPointer -> pin) 
		  (progn
		    ;; Align center point (edges might get off grid!!!)
		    ;; Label does not follow ...
		    (printf "You are aligning pins, notice that edges might come off-grid! \n")
		    (setq hWidth   (xstop - xstart)/2.0)
		    (setq hHeight  (ystop - ystart)/2.0)
		    (setq xmidG    xSnap*round((xstart+hWidth)/xSnap))
		    (setq ymidG    ySnap*round((ystart+hHeight)/ySnap))		
		    (setq xstartG    xmidG - hWidth)
		    (setq ystartG    ymidG - hHeight)
		    (setq xstopG     xmidG + hWidth)
		    (setq ystopG     ymidG + hHeight)))
	      (if (equal xyDir   "x")  
		  dbPointer->bBox = (list (list xstartG ystart)  (list xstopG ystop)))
	      (if (equal xyDir   "y")  
		  dbPointer->bBox = (list (list xstart ystartG)  (list xstop ystopG)))
	      (if (equal xyDir   "xy") 
		  dbPointer->bBox = (list (list xstartG ystartG) (list xstopG ystopG))))))

(defun daisyBbox (cellView)
  (let ((bbox nil))
    (if (member "instance" cellView->shapes~>layerName)
	(foreach shape cellView->shapes
		 (if (and (equal shape->objType "rect") (equal shape->layerName "instance"))
		     (setq bBox shape->bBox)))
      (setq bBox cellView->bBox))
    bBox))

(defun daisySquareDance ( @optional (xDir t))
;; Places all the components in a square.
  (let ((selected nil) 
	(noElements 0) (zyx (list 0 0))
	(columns 0) (rows 0)
	(startX 0) (startY 0)
	(deltaX 0) (deltaY 0)
	(minDist 0) (maxDist 1e9))
    (setq selected (geGetSelectedSet))
    (setq noElements (length selected))
    (setq columns (ceiling (pow noElements 0.5)))
    (setq rows    (ceiling noElements / columns))
    
    (if nil (printf "%d %d \n" columns rows))
    (if selected 
	(progn
	  (setq objectX (car selected))
	  (setq startX (car objectX->xy))
	  (setq startY (cadr objectX->xy))
	  (setq deltaX (abs ((caar (daisyBbox objectX->master)) - (caadr (daisyBbox objectX->master)))))
	  (setq deltaY (abs ((cadar (daisyBbox objectX->master)) - (cadadr (daisyBbox objectX->master)))))))

    (for row 0 columns-1
	 (for col 0 columns-1
	      (when selected 
		(setq zyx (list startX+deltaX*(if xDir  col row) startY+deltaY*(if xDir row col)))
		(if nil (printf "%L \n" zyx))
		(car selected)->xy = zyx
		(setq selected (cdr selected)))))
    ))

(defun daisyTranspose ()
;; Transposing a list of object (without rotating them)

   (let (( selected nil) (noElements 0) (columns 0) (dbPointer nil)) 
    (setq selected (geGetSelectedSet))
    (setq noElements (length selected))
    (setq columns (ceiling (pow noElements 0.5)))
    (foreach dbObject (geGetObjectSelectedSet)
	     (setq dbPointer (car dbObject))
	     (dbPointer ->  xy = (reverse dbPointer->xy)))))

(defun distributeXY (xyDir @optional (verbose t))  
  ;; Warning objects do not become ordered. "Significance" given 
  ;; by the selection order.  
  (let ( (maxX -1e9) (minX 1e9) (maxY -1e9) (minY 1e9) 
	 (len_N 0) (x 0) (y 0) (dbPointer nil) (dbType nil)
	 (sumX_p 0) (sumY_p 0) (lenN_p 0))    
    (setq len_N (length (geGetObjectSelectedSet)))    
    (if (greaterp len_N  1) then
      (foreach dbObject (geGetObjectSelectedSet) 
	       (setq dbPointer (car dbObject))
	       (setq dbType dbPointer->objType)	       
	       (if (member dbType (list "inst" "mosaicInst" "mosaic")) then
		 (setq x (car dbPointer -> xy))
		 (setq y (cadr dbPointer -> xy))	     
		 (if (greaterp x  maxX) (setq maxX  x))
		 (if (lessp x  minX)    (setq minX  x))
		 (if (greaterp y  maxY) (setq maxY  y))
		 (if (lessp y  minY)    (setq minY  y)) t) 
	       (if (equal dbType "path") then 
		 (setq sumX_p 0)
		 (setq sumY_p 0)
		 (setq lenN_p dbPointer -> nPoints)		 
		 (foreach dbPoints dbPointer->points 
			  (setq sumX_p  sumX_p +  (car dbPoints))
			  (setq sumY_p  sumY_p +  (cadr dbPoints)) t)
		 (setq x  sumX_p / lenN_p)
		 (setq y  sumY_p / lenN_p)
		 (if (greaterp x  maxX) (setq maxX  x))
		 (if (lessp    x  minX) (setq minX  x))
		 (if (greaterp y  maxY) (setq maxY  y))
		 (if (lessp    y  minY) (setq minY  y))))
      (setq deltaX  (maxX - minX ) / float(len_N-1))
      (setq deltaY  (maxY - minY) / float(len_N-1))
      (if verbose
	  (progn
	    (printf "x: %f %f %f\n " maxX minX deltaX)
	    (printf "y: %f %f %f\n " maxY minY deltaY)))      
      (setq n 0)      
      (foreach  dbObject geGetObjectSelectedSet() 
		(setq dbPointer   (car dbObject))
		(setq dbType      dbPointer ->  objType)		
		(if (member dbType (list "inst" "mosaicInst" "mosaic")) then
		  (setq x (car dbPointer -> xy))
		  (setq y (cadr dbPointer -> xy))
		  (if (equal xyDir "x") dbPointer -> xy =  (list minX+deltaX*n y))
		  (if (equal xyDir "y") dbPointer -> xy =  (list x minY+deltaY*n)) t)
		(if (equal dbType "path") then 
		  (setq sumX_p  0)
		  (setq sumY_p  0)
		  (setq lenN_p  dbPointer -> nPoints)
		  (foreach  dbPoints dbPointer->points 
			    (setq sumX_p sumX_p +  (car dbPoints))
			    (setq sumY_p sumY_p +  (cadr dbPoints)))
		  (setq avgX_p sumX_p / lenN_p)
		  (setq avgY_p sumY_p / lenN_p)
		  (setq xList_p  (list))
		  (setq yList_p  (list))
		  (foreach dbPoints dbPointer->points 
			   (setq x (car dbPoints))
			   (setq y (cadr dbPoints))
			   (setq xList_p  (append xList_p list(list(x-avgX_p+minX+n*deltaX y))))
			   (setq yList_p  (append yList_p list(list(x y-avgY_p+minY+n*deltaY)))))
		  (if (equal xyDir "x") dbPointer->points = xList_p)
		  (if (equal xyDir "y") dbPointer->points = yList_p))
		(setq n n+1) t) t) )
    (daisyGridObjectXY "xy" 1) t)
  
(defun setGridXY (xyDir snapValue minorGrid majorGrid)
  (let ( ( cw (hiGetCurrentWindow) ) )
    ;; Set snap grid.
    (if (equal xyDir  "x")      cw->xSnapSpacing = snapValue)
    (if (equal xyDir  "y")      cw->ySnapSpacing = snapValue)
    (if (equal xyDir  "xy") then     
      cw->xSnapSpacing = snapValue
      cw->ySnapSpacing = snapValue)    
    cw->gridSpacing = minorGrid
    cw->gridMultiple = majorGrid    
    (envSetVal "layout" "ySnapSpacing" 'float snapValue)
    (envSetVal "layout" "xSnapSpacing" 'float snapValue)
    (printf "Note: %s snap spacing set to %f micron" xyDir snapValue) ) t)

(defun daisyReshapeTran (objId action)
  ;; This is just a template function and should be process dependent. 
  ;; In the end, it is going to be overwritten by same function name
  ;; in the $DAISYAREA/pdkSpecific/$CDSPROCESSNAME/skill/... 
  ;; 
  (let ((noFingers 0) (totalWidth 0) (fingerWidth 0))

  (if (member objId->cellName (list "nmos1v" "pmos1v"))
      ;; Now, we know it is a transistor
      (progn
      noFingers = objId->fingers
      totalWidth =  objId->w
      fingerWidth = objId->fw   
      (if (equal action "incr")
	  (progn
	    objId->fingers = objId->fingers + 1
	    objId->fw = totalWidth / objId->fingers))))))

(defun daisyGrowObjectXY (@optional (dir "up") (obj (geGetSelSet)))
  (let ( ( scale 0.9 ) (grid 0.005) (trangrid 10e-9) ) 	     
    (foreach obj (if (listp obj) obj (list obj))
	     
	     (case obj->objType
		   ("path"
		    (progn  obj->width = grid*(floor (obj->width)*(if (equal dir "up") 1.0/scale scale)/grid)))
		   ("inst"
		    ;; Quick and dirty to illustrate for the course. 
		    ;; This should be encapsulated in function and associated with the pdk settings.
		    (progn (if (member obj->cellName (list "nmos1v"))
			       obj->fw = trangrid*(floor obj->fw*(if (equal dir "up") 1.0/scale scale)/trangrid))))

		   ))))


(defun daisyMoveObjectXY (xyDir distance_D)	  
  ;; Takes a selected set of graphical objects and moves them
  (let (dbPointer_D dbType_D l_transform_D x_D y_D orc_D mag_D)    
    (foreach dbObject (geGetObjectSelectedSet) 
	     (setq dbPointer_D (car dbObject))
	     (setq dbType_D  dbPointer_D->objType)	     
	     (if (member dbType_D (list "inst" ))  then       
	       (setq l_transform_D dbPointer_D->transform)
	       (setq x_D   (caar  l_transform_D))
	       (setq y_D   (cadar l_transform_D))
	       (setq orc_D (cadr  l_transform_D))
	       (setq mag_D (caddr l_transform_D))     	       
	       (if (equal xyDir "x") (setq x_D x_D+distance_D))
	       (if (equal xyDir "y") (setq y_D y_D+distance_D))
	       (if (equal xyDir "xy") then
		 (setq x_D x_D+distance_D)
		 (seyq y_D y_D+distance_D))   
	       dbPointer_D->transform =  (list (list x_D y_D) orc_D mag_D)
	       else
	       (if (equal xyDir "x")       
		   (dbMoveFig dbPointer_D nil (list (list distance_D 0) "R0")))
	       (if (equal xyDir "y")       
		   (dbMoveFig dbPointer_D nil (list (list 0 distance_D) "R0")))
	       (if (equal xyDir "xy")       
		   (dbMoveFig dbPointer_D nil (list (list distance_D distance_D) "R0")))))))

(defun daisyLeCreateContact (cellId typeList posList width 
				  @optional (height 0) (masterId nil) (netName nil) (rot "R0") (xBias "center") (yBias "center"))
  (let ((holeSize 0.12) (type nil) (deltaXY 0.26) 
	(contactList nil) (viaId nil))
    (if (equal height 0) 
	(setq height width))  
    (if (not (or (lessp height 0) (lessp width 0)))
	(progn
	  (foreach type (if (not (listp typeList)) 
			    (list typeList) typeList)
		   (if (equal type "L1M1_C") (setq deltaXY 0.45))
		   (if (or (equal type "M2_M1") (equal type "M3_M2"))
		       (progn (setq holeSize 0.13) (setq deltaXY 0.30)))
		   
		   (if (or (equal type "M1_PO" ) (equal type "DFM_M1_PO" ))
		       (progn (setq holeSize 0.12) (setq deltaXY 0.28)))
		   
		   (setq contactList 
			 (append     
			  (list (setq viaId 
				      (leCreateContact cellId type posList rot 
						       holeSize holeSize
						       (daisyLeviaSize width height type ?cols nil)
						       (daisyLeviaSize width height type ?rows nil)
						       deltaXY deltaXY xBias yBias)))
			  contactList))
		   (if masterId (leAttachFig viaId masterId))       
		   (if netName (dbCreateProp viaId "netName" "string" netName)))
	  (if (listp typeList) contactList (car contactList))))))

(defun daisyLeviaSize (width height type @key (rows t) (cols t)) 
  (let ( (holeSize 0.13) (a nil) (deltaXY 0.30) )
    (setq width (plus width 1e-6))
    (setq height (plus height 1e-6))    
    (if (equal type "L1M1_C") (setq deltaXY 0.45))
    (if (or (equal type "M1M2_C") (equal type "M2M3_C"))
	(progn (setq holeSize 0.23) (setq deltaXY 0.46)))
    (if (equal type "M3M4_C")
	(progn (setq holeSize 0.30) (setq deltaXY 0.62)))
    (if (and rows cols)       
	(setq a (list 
		 (int (floor ((height-holeSize)/deltaXY+1)))
		 (int (floor ((width-holeSize)/deltaXY+1))))))
    (if (and rows (not cols)) 
	(setq a (int (floor ((height-holeSize)/deltaXY+1)))))
    (if (and (not rows) cols) 
	(setq a (int (floor ((width-holeSize)/deltaXY+1)))))
    a))

(defun daisyLeWireLength (l_points)
  (let (( wireLength 0))
    
    (setq lastX (caar l_points))
    (setq lastY (cadar l_points))
    
    (foreach xyPair (cdr l_points)	     
	     (setq xVal (car xyPair))
	     (setq yVal (cadr xyPair))	     
	     (setq xDist (xVal - lastX))
	     (setq yDist (yVal - lastY))	     
	     (setq lastX xVal)
	     (setq lastY yVal)
	     (setq xyDist (sqrt (xDist**2 + yDist**2)))	     
	     (setq wireLength (plus wireLength xyDist)))    
    wireLength))

(defun daisyLeSpaceWire (space l_points)
  (let ( (distTot 0) (distDone 0) 
	 (distLeft 0) (xVal 0) (yVal 0) 
	 (xDist 0)  (yDist 0) (xyDist 0) 
	 (sampledList (list)) (wireLength 0))
    
    (setq lastX (caar l_points))
    (setq lastY (cadar l_points))
    (setq distLeft 0)
    (setq distTot 0)
    (setq distDone 0)
    (setq wireLength (daisyLeWireLength l_points))
  
    (foreach xyPair (cdr l_points)
	   (setq xVal (car xyPair))
	   (setq yVal (cadr xyPair))
	   
	   (setq xDist (xVal - lastX))
	   (setq yDist (yVal - lastY))
	   
	   (setq xyDist (sqrt (xDist**2 + yDist**2)))
	   (setq deltaX xDist/xyDist)
	   (setq deltaY yDist/xyDist)
	   (setq distLeft xyDist+distLeft)
	   
	   (while (greaterp distLeft 0)
	     (if (and (greaterp distDone space*0.9) 
		      (lessp distDone (wireLength-space)))
		 (setq sampledList
		       (append 
			(list (list lastX+(distDone-distTot)*deltaX 
				    lastY+(distDone-distTot)*deltaY))
			sampledList)))
	     (setq distDone distDone+space)    
	     (setq distLeft distLeft-space))
	   (setq lastX xVal)
	   (setq lastY yVal)
	   (setq distTot distTot + xyDist))
    sampledList))

(defun daisyLeCreatePath (d_cellView tx_layer l_points x_width
				   @key 
				   (masterId nil)
				   (name nil)
				   (addToNet nil)
				   (manyLabels t)
				   (vias nil)
				   (viaDimensions nil)
				   (pin nil))
  ;; (viaDimensions (list (list 0.3 0.3) (list 0.3 0.3)))
  (let ( ( pathId nil) (netId nil) (endDirection nil) )
    
    (if (or (caar (reverse l_points)) > (caar (cdr (reverse l_points)))
	    (caar (reverse l_points)) < (caar (cdr (reverse l_points))))
	(setq endDirection "R0") (setq endDirection "R90"))
    (setq pathId 
	  (daisyLePath d_cellView tx_layer l_points x_width masterId name name nil))
    (if name 
	(progn
	  (foreach xyPoint (if manyLabels l_points (list (car l_points) (car (last l_points))))
		   (daisyLeCreateLabel d_cellView tx_layer xyPoint name (min x_width 5) pathId))
	  (if addToNet 
	      (progn
		(setq netId (dbMakeNet d_cellView name))
		(printf "%s \n" name)
		(dbCreateProp pathId "netName" "string" name)
		(dbAddFigToNet pathId netId )
		)
	    )
	  (if pin 
	      (daisyLeCreatePin d_cellView 
			      ?position (car (last l_points))
			      ?pinName name
			      ?pinLayer tx_layer
			      ?labelRot endDirection
			      ?labelSize (min x_width 5)
			      ?dimensions (if (equal endDirection "R0") 
					      (list 0.10 x_width) 
					    (list x_width 0.10))
			      ?masterId pathId))))
    (if vias 
	(progn
	  (if (car vias) 
	      (daisyLeCreateContact d_cellView (car vias) (car l_points) 
				  (if viaDimensions (caar viaDimensions) x_width) 
				  (if viaDimensions (cadar viaDimensions) x_width) pathId name))
	  (if (cadr vias) 
	      (daisyLeCreateContact d_cellView (cadr vias) (car (last l_points) )
				  (if viaDimensions (caadr viaDimensions) x_width) 
				  (if viaDimensions (cadadr viaDimensions) x_width) pathId name))))
    
    pathId))


(defun daisyLePath (d_cellView tx_layer l_points x_width 
			     @optional (masterId nil) 
			     (netId nil) (netName nil) (showLabel nil))
  (let ( (pathId nil) )
    (setq pathId 
	  (dbCreatePath d_cellView tx_layer l_points x_width))
    (if masterId (leAttachFig pathId masterId))
    (if netName (dbCreateProp pathId "netName" "string" netName))    
    (if netId 
	(dbAddFigToNet 
	 pathId    
	 (if (stringp netId) (dbMakeNet d_cellView netId) netId)))
    (if showLabel 
	(leAttachFig     
	 (dbCreateLabel d_cellView  (list (if (listp tx_layer) (car tx_layer) tx_layer) "label")
			(car l_points)
			(if netId (if (stringp netId) netId netId->"name") (if netName netName "someNet"))
			"centerCenter"  
			"R0" "stick" 0.2)
	 pathId))    
    pathId))

(defun daisyLeRect (d_cellView tx_layer l_bBox 
			     @optional (masterId nil) (netId nil) 
			     (chipName nil) 
			     (textLabel nil) (nameLabel nil))
  (let ( (rectId nil) )
    (setq rectId 
	  (dbCreateRect d_cellView tx_layer l_bBox))
    (if masterId (leAttachFig rectId masterId))
    (if netId 
	(dbAddFigToNet 
	 rectId    
	 (if (stringp netId) (dbMakeNet d_cellView netId) netId)))
    (if chipName (dbCreateProp rectId "chipName" "string" chipName))
    (if textLabel	
	(leAttachFig     
	 (dbCreateLabel d_cellView  "text"
			(daisyCenterPoint l_bBox 0.005)
			textLabel "centerCenter"  
			(if (greaterp (daisyRectWidth l_bBox) (daisyRectHeight l_bBox)) "R0" "R90")
			"stick" 
			0.8*(if (greaterp (daisyRectWidth l_bBox) (daisyRectHeight l_bBox)) 
				(daisyRectHeight l_bBox) (daisyRectWidth l_bBox)))
	 rectId))
    (if nameLabel	
	(leAttachFig     
	 (dbCreateLabel d_cellView  (list tx_layer "label")
			(daisyCenterPoint l_bBox 0.005)
			textLabel "centerCenter"  
			(if (greaterp (daisyRectWidth l_bBox) (daisyRectHeight l_bBox)) "R0" "R90")
			"stick" 
			0.4*(if (greaterp (daisyRectWidth l_bBox) (daisyRectHeight l_bBox)) 
				(daisyRectHeight l_bBox) (daisyRectWidth l_bBox)))
	 rectId))

    rectId))
 
(defun placePin (d_cellView pos name @optional (dir "input") (index nil))
  (daisyLeCreatePin d_cellView 
		  ?position pos
		  ?labelSize 1
		  ?pinDirection dir
		  ?pinName (if index (sprintf nil (strcat name "<%d>") index) name)
		  ?pinLayer (if (or (equal (car pos) 0) (equal (car pos) 300)) "M3" "M2")
		  ?labelRot (if (or (equal (car pos) 0) (equal (car pos) 300)) "R0" "R90")))

(defun daisyLeCreatePin 
  (cellId @key 
	  (position    (list 0.0 0.0)) 
	  (pinLayer    "M1") 
	  (pinName     "DATA") 
	  (pinDirection "inputOutput")
	  (labelAlign  "centerCenter")
	  (labelRot    "R0")
	  (verbose     t)
	  (dimensions  nil)
	  (labelOffset (list 0 0))
	  (labelSize   0.2)
	  (masterId    nil))
  
  (let ( (xPosJ        (car   position) )
	 (yPosJ        (cadr  position) )
	 (labelLayer  "M1" )
	 (rectId      nil)
	 (termId      nil)
	 (pinId       nil)
	 (netId       nil)
	 (pinDim     (list
		      (list "PO" (list 0.10 0.10))
		      (list "M1" (list 0.12 0.12))
		      (list "M2" (list 0.14 0.14))
		      (list "M3" (list 0.14 0.14))
		      (list "M4" (list 0.14 0.14))
		      (list "M5" (list 0.14 0.14))
		      (list "M6" (list 0.42 0.42))
		      (list "M7" (list 0.42 0.42))
		      (list "M8" (list 0.42 0.42))
		      (list "MD" (list 0.42 0.42))
		      (list "AP" (list 3.00 3.00))
		      (list "pad"  (list 0.40 0.40)))))
    (if (not dimensions)
	(setq dimensions 
	      (cadr (assoc pinLayer pinDim))))
    
    (setq labelName pinName)  
    (setq netId (dbMakeNet cellId pinName))
    (if verbose (printf "Able to create NET -> %s\n" netId->name ))
    (setq rectId  
	  (dbCreateRect cellId (list pinLayer "pin") 
			(list 
			 (list xPosJ-(car dimensions)/2 yPosJ-(cadr dimensions)/2)
			 (list xPosJ+(car dimensions)/2 yPosJ+(cadr dimensions)/2))))
    (setq termId  (dbCreateTerm  netId pinName pinDirection))
    (if (not termId) (setq termId (dbFindTermByName cellId pinName)))
    (if verbose (printf "Able to create TERM -> %s\n" termId->name ))
    (setq pinId (dbCreatePin netId rectId pinName))
    (if (not pinId) 
	(if verbose (printf "===========================> Pin already exists? \n")))
    (if verbose (printf "%L" labelLayer))
    (leAttachFig     
     (setq labelId 
	   (dbCreateLabel cellId (list pinLayer "pin")
			  (list xPosJ+(car labelOffset) yPosJ+(cadr labelOffset))
			  pinName labelAlign labelRot "stick" labelSize))
     rectId)
    (if masterId (leAttachFig rectId masterId))
    rectId))


(defun daisyLeAttachFig (objectId masterId)
  
  (if (listp objectId)
      (foreach objectIdx objectId
	       (if (and masterId objectIdx)
		   (leAttachFig objectIdx masterId)))
    (if (and masterId objectId)
	(leAttachFig objectId masterId))))

(defun daisyLeObjectInfo (@optional (langMode "skill")) 
  (let ( (contList (list "AP_M6" "AP_M6c" "DFM_M1_NODc" 
			 "DFM_M1_NPOc" "DFM_M1_NW" 
			 "DFM_M1_OD" "DFM_M1_PO" 
			 "DFM_M1_PODc" "DFM_M1_PPOc" 
			 "DFM_M1_SUB" "DFM_M2_M1" 
			 "DFM_M2_M1c" "DFM_M3_M2" 
			 "DFM_M3_M2c" "DFM_M4_M3" 
			 "DFM_M4_M3c" "M1_NODc" 
			 "M1_NPOc" "M1_NW" "M1_OD" 
			 "M1_PO" "M1_PODc" "M1_PPOc" 
			 "M1_SUB" "M2_M1" "M2_M1c" 
			 "M3_M2" "M3_M2c" "M4_M3" 
			 "M4_M3c" "M5_M4" "M5_M4c" 
			 "M6_M5" "M6_M5c" "MD_M6" 
			 "MD_M6c"))
	 (kalle "" )
	 (dbPointer nil) 
	 (command "")
	 (commandList (list )) 
	 (dbType nil))
    (if (equal langMode "skill")
	(progn
	  (foreach dbObject (geGetObjectSelectedSet) 
		   (setq dbPointer (car dbObject))
		   (setq dbType dbPointer->objType)
		   (setq kalle "")		   
		   (if (equal dbType "path") 
		       (progn
			 (foreach xyPair dbPointer->points
				  (sprintf kalle "%s (list %4.3f %4.3f) " kalle (car xyPair) (cadr xyPair)))
			 (sprintf command 
				  "(%s %s (list %s) %f nil %s)" 
				  "daisyLePath cellId " 
				  (sprintf nil "(list \"%s\" \"%s\")" (car dbPointer->lpp) (cadr dbPointer->lpp))
				  kalle
				  dbPointer -> width
				  (if dbPointer -> net (sprintf nil "\"%s\"" dbPointer->net->name) ""))
			 (printf "%s\n" command)
			 (setq commandList (append commandList (list command)))))
		   (if (equal dbType "rect") 
		       (progn
			 (foreach xyPair dbPointer->bBox
				  (sprintf kalle "%s (list %4.3f %4.3f) " kalle (car xyPair) (cadr xyPair)))
			 (sprintf command 
				  "(%s %s (list %s) nil %s)" 
				  "daisyLeRect cellId " 
				  (sprintf nil "(list \"%s\" \"%s\")" (car dbPointer->lpp) (cadr dbPointer->lpp))
				  kalle
				  (if dbPointer -> net (sprintf nil "\"%s\"" dbPointer->net->name) ""))
			 (printf "%s\n" command)
			 (setq commandList (append commandList (list command)))))
		   
		   (if (equal dbType "polygon") 
		       (progn
			 (foreach xyPair dbPointer->points
				  (sprintf kalle "%s (list %4.3f %4.3f) " kalle (car xyPair) (cadr xyPair)))
			 (sprintf command 
				  "(%s %s (list %s))" 
				  "dbCreatePolygon cellId " 
				  (sprintf nil "(list \"%s\" \"%s\")" (car dbPointer->lpp) (cadr dbPointer->lpp))
				  kalle)
			 (printf "%s\n" command)
			 (setq commandList (append commandList (list command)))))
		   
		   
		   (if (equal dbType "inst")
		       (if (and (equal dbPointer->libName "tsmcN80") 
				(member dbPointer->cellName contList)
				;; (equal (car (parseString dbPointer->cellName "_")) "via")
				)
			   (progn
			     (sprintf command 
				      "(%s \"%s\" (list %s) %4.3f %4.3f nil nil \"%s\")"
				      "daisyLeCreateContact cellId"
				      dbPointer->cellName 
				      (sprintf nil "%4.3f %4.3f" (car dbPointer->xy) (cadr dbPointer->xy))
				      (abs (caadr dbPointer->bBox)-(caar dbPointer->bBox))
				      (abs (cadadr dbPointer->bBox)-(cadar dbPointer->bBox))
				      dbPointer->orient
				      )
			     (printf "%s\n" command)
			     (setq commandList (append commandList (list command)))
			     ) ; ** progn **
			 (progn
			   (sprintf command 
				    "(%s \"%s\" \"%s\" \"%s\" \"%s\" (list %s) \"%s\")"
				    "dbCreateInstByMasterName cellId "
				    dbPointer -> libName
				    dbPointer -> cellName
				    dbPointer -> viewName
				    dbPointer -> name
				    (sprintf nil "%4.3f %4.3f" (car dbPointer->xy) (cadr dbPointer->xy))
				    dbPointer->orient
				    )
			   (printf "%s\n" command)
			   (setq commandList (append commandList (list command)))
			   ))))))
    
    (if (length commandList) > 1 then (daisyDisplayList commandList))
    
    ))

(defun daisyLeCreateRulerMarker () 

  ;; Creates a marker in the layout using the ruler

  ;; (hiGetScreenPoint)
  (let ((deltaX 0) (deltaY 0) (xPos 0) (yPos 0))
    
    ;; (hiGetViewBBox)
    (setq deltaX (times 0.1 (caadr (hiGetViewBBox))-(caar (hiGetViewBBox))))
    (setq deltaY (times 0.1 (cadadr (hiGetViewBBox))-(cadar (hiGetViewBBox))))
    (setq xPos (times 0.001 (car (hiGetDbuPoint (hiGetCurrentWindow)))))
    (setq yPos (times 0.001 (cadr (hiGetDbuPoint (hiGetCurrentWindow)))))
    (foreach xOff (list 0 (times 0.1 deltaX) -(times 0.1 deltaX))
	     (foreach yOff (list 0 (times 0.1 deltaY) -(times 0.1 deltaY))
		      (leCreateRuler (hiGetCurrentWindow)->cellView  
				     (list (list xPos-deltaX+xOff yPos-deltaY+yOff)
					   (list xPos+deltaX+xOff yPos+deltaY+yOff)))
		      (leCreateRuler (hiGetCurrentWindow)->cellView  
				     (list (list xPos+deltaX+xOff yPos-deltaY+yOff)
					   (list xPos-deltaX+xOff yPos+deltaY+yOff)))))))
    


;;;(list (list 0 0) (list 0 10))))

(defun daisyLeRemoveLabelHierarchy (cellView @optional 
					   (beenThereList (list )))  
  (let ( (notBeenThereList nil) (charlie 0) (cw2 nil) (shape nil))
    (foreach inst cellView->instances	  
	     charlie = charlie +1 
	     (if (member inst->cellName beenThereList)
		 (progn 
		   ;; Do something else here if needed...
		   ;; (printf "%s : I've been there!!! \n" inst->cellName)
		   t)
	       (progn	       
		 ;; Do the stuff here...	   
		 (setq beenThereList (append beenThereList (list inst->cellName)))
		 (printf "%d. CellName: %s \n" charlie inst->cellName)	       		
 		 (setq cw2 (dbOpenCellViewByType 
			    inst->libName inst->cellName inst->viewName "" "a"))
		 (foreach shape cw2->shapes
			  (if (equal shape->objType "label")
			      (progn
				(if (getq shape associate) 
				    (printf "--> Label %s in %s found.\n" 
					    shape->associate->name inst->cellName)
				  (printf "--> Label %s in %s found.\n" 
					  shape->theLabel inst->cellName))
				(dbDeleteObject shape))))
		 (dbSave cw2) (dbClose cw2)
		 (setq notBeenThereList 
		       (daisyLeRemoveLabelHierarchy inst->master beenThereList))		 
		 ;; Crude but simple ...
		 (foreach cell notBeenThereList
			  (if (not (member cell beenThereList)) 
			      (setq beenThereList 
				    (append beenThereList (list cell)))))))))
  
  beenThereList)

(defun daisyLefixLabels (cellView @key 
				(labelsToRemove nil) 
				(labelsToKeep nil) 
				(labelsToRename nil)
				(hierarchy t))
  (let ( (labelList nil) (labelName ""))
    (if hierarchy 
	(setq labelList (daisyLeRemoveLabelHierarchy cellView)))
    (lePrintLabels cellView)
    
    (foreach shape cellView->shapes
	     (if (equal shape->objType "label")
		 (progn 
		   (setq labelName (if (getq shape associate) 
				       shape->associate->name 
				     shape->theLabel))
		   (if (not (member labelName labelsToRemove))
		       (progn 
			 (if (assoc labelName labelsToRename) 
			     (setq labelName (cadr (assoc labelName labelsToRename))))
			 (daisyLeCreatePin cellView
					 ?position  shape->xy
					 ?pinLayer  shape->layerName
					 ?pinName  labelName)))
		   (dbDeleteObject shape))))))


(defun daisyLecreateBusLabels (oldName newName start stop 
				     @key (formatted nil))
  (let ((renameList nil))
    (while (lessp start stop+1)
      (setq renameList 
	    (append renameList 
		    (list  
		     (if (not formatted)
			 (list (sprintf nil "%s%d" oldName start)
			       (sprintf nil "%s<%d>" newName start))
		       (list (sprintf nil oldName start)
			     (sprintf nil newName start))))))
      (preincrement start))
    renameList))



(defun lePolygize (regionType)		  
  (let ( (objects  (geGetObjectSelectedSet))
	 (cw       (hiGetCurrentWindow)) )   
    (foreach  sObj objects
	      (setq sObjP (car sObj))
	      (setq sObjType sObjP->objType)	      
	      (if (equal sObjType "path") then
		;; A path has been detected and therefore perform the action
		(printf "A %s has been detected. \n" sObjType)
		(setq sObjnP sObjP->nPoints)		
		(if (sObjnP > 2) 
		    (progn
		      (setq sObjPts sObjP->points)
		      (setq sObjPts 
			    (append sObjPts (list (list (caar sObjPts) 
							(cadar (last sObjPts))))))
		      (setq l_layer (list))		  
		      (if (equal regionType "hvpfet") (setq l_layer (list "od" "nwell")))
		      (if (equal regionType "hvnfet") (setq l_layer (list "od")))
		      (foreach layer l_layer
			       (printf "Convex polygon of layer %s added.\n" layer) 
			       (dbCreatePolygon cw->cellView layer sObjPts)))
		  (printf "Path with only two points detected. No measures taken. \n"))))))

(defun leDisplaySomeLayersOnly (layerPairList)
  (if (not layerPairList)                             ; Metal only   
      (setq layerPairList GLOBALLAYERPAIRLIST))
  (if (not (listp layerPairList))
      (if (equal layerPairList "all") (leSetAllLayerVisible t))
    (progn
      (setq currentLayer (leGetEntryLayer))
      (leSetAllLayerVisible nil)
      (foreach layerPair layerPairList
	       (foreach type (list "pin" "drawing" "label" )
			(leSetLayerVisible  (list (car layerPair) type) t)))
      (leSetEntryLayer currentLayer))))

(defun lePrintLabels  ( @optional (cellView nil))
  (let ((layerPairList GLOBALLAYERPAIRLIST)	
	(instSel (leIsInstSelectable))
	(labelList (list )) (groundList (list )) (supplyList (list ))
	(pinSel (leIsPinSelectable))
	(oport nil)
	(wind (hiGetCurrentWindow)))        
    (leSetEntryLayer (car layerPairList))
    (leSetAllLayerVisible nil)
    (foreach layerPair layerPairList
	     (leSetLayerVisible  layerPair t))
    (leSetInstSelectable nil)
    (leSetPinSelectable nil)
    (if (not cellView)
	(progn
	  (geDeselectAll)
	  (geSelectAll))
      (progn
	(geDeselectAllFig cellView)
	(geSelectAllFig cellView)))    
    (foreach label (geGetSelectedSet)
	     (if (getq label associate) 
		 (setq labelName label->associate->name) 
	       (setq labelName label->theLabel))
	     (printf "%s \n" labelName)	     
	     (if (or (rexMatchp "gnd" labelName) 
		     (rexMatchp "vss" labelName) 
		     (rexMatchp "GND" labelName)) 
		 (setq groundList (append (list labelName) groundList)))
	     (if (or (rexMatchp "vpwr" labelName) 
		     (rexMatchp "vdd" labelName)
		     (rexMatchp "VDD" labelName)) 
		 (setq supplyList (append (list labelName) supplyList)))
	     ;; Crude ...
	     (if (not (or  (rexMatchp "gnd" labelName) 
			   (rexMatchp "vss" labelName) 
			   (rexMatchp "GND" labelName) 
			   (rexMatchp "vpwr" labelName) 
			   (rexMatchp "vdd" labelName)))
		 (setq labelList (append (list labelName) labelList)))
	     ;; Sort ... it is sort of nice ...
	     (setq labelList 
		   (reverse (sort labelList `(lambda (x y) (equal 1 (strcmp x y))))))
	     (setq groundList 
		   (reverse (sort groundList `(lambda (x y) (equal 1 (strcmp x y))))))
	     (setq supplyList 
		   (reverse (sort supplyList `(lambda (x y) (equal 1 (strcmp x y)))))))
    (if groundList (progn 
		     (printf "Ground Nets: ")
		     (foreach label groundList (printf "%s " label))
		     (printf "\n")))
    (if supplyList (progn 
		     (printf "Supply Nets: ")
		     (foreach label supplyList (printf "%s " label))
		     (printf "\n")))
    (if labelList (progn 
		    (printf "Signal Pins and Nets: ")
		    (foreach label labelList (printf "%s " label))
		    (printf "\n")))    
    (leSetAllLayerVisible t)
    (leSetInstSelectable instSel)
    (leSetPinSelectable pinSel)
    (geDeselectAll)))

(defun daisyLabelsToPins (@optional (cv nil))
  (let ((djjd nil))
    (if (not cv) (setq cv (hiGetCurrentWindow)->cellView))
    (foreach shape cv->shapes
	     (if (equal shape->purpose "pin")		
		 (progn
		   (printf "%s : %L \n" shape->theLabel shape->xy)
		   (if (equal shape->theLabel "VDD")
		       (progn
			 shape->theLabel = "vdd!"
			 ))
		   
		   (if (equal shape->theLabel "VSS")
		       (progn
			 shape->theLabel = "gnd!"
			 ))
		   (daisyLeCreatePin 
		    cv
		    ?position    shape->xy
		    ?pinLayer    shape->layerName
		    ?pinName     shape->theLabel
		    ?pinDirection "inputOutput"
		    ?labelAlign  "centerCenter"
		    ?labelRot    "R0"
		    ?verbose     nil
		    ?dimensions  (list 0.06 0.06)
		    ?labelOffset (list 0 0)
		    ?labelSize   shape->height
		    ?masterId    shape))))))

(defun daisyLabelsToPinsLib (@optional (libName  "tcbn40lpbwphvt_110a"))
  (let ((layoutView nil) 
	(symTermList (list )) (currentLib nil) (hitMe nil)
	(vpwrPort nil) (vgndPort nil) (vddlPort nil) (aList nil))
    
    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))
    (foreach cell  currentLib~>cells
	     (setq layoutView (dbOpenCellViewByType cell->lib->name cell->name "layout" nil "a"))	     
	     (if layoutView 
		 (progn 
		   (daisyLabelsToPins layoutView)
		   (dbSave layoutView)
		   (dbClose layoutView))))))

(defun daisyCopyBoundary (@optional (cellView nil))
  (let ((shapeId nil) (cellId nil) (instId nil) (instIdy nil))
    (if (not cellView)
	(setq cellView (hiGetCurrentWindow)->cellView))
    (if cellView
	(progn
	  (setq cellId (dbOpenCellViewByType cellView->lib->name cellView->cell->name "layout" nil "a"))
	  (if cellView->shapes
	      (if (member "prBoundary" cellView->shapes~>layerName)
		  (progn 
		    (foreach shah  cellView->shapes
			     (if (and (equal shah->objType "rect") 
				      (equal shah->lpp (list "prBoundary" "drawing")))
				 (progn
				   (printf "Adding instance and prBoundary:by rectangles to the layout of %s in %s. \n"  
					   cellView->cell->name    
					   cellView->lib->name)
				   (setq instId (dbCopyFig shah cellId))
				   (setq instIdy (dbCopyFig shah cellId))				   ))
			     (if (or (and  (equal shah->objType "rect") (equal shah->layerName "instance"))
				     (and  (equal shah->objType "rect") (equal shah->lpp (list "prBoundary" "boundary"))))
				 (progn
				   (printf "Deleting old instance and boundary:by layers in the layout of %s in %s. \n"  
					   cellView->cell->name    
					   cellView->lib->name)
				   (dbDeleteObject shah cellId)
				   ))
			     (if instId 
				 instId->layerName = "instance")
			     (if instIdy 
				 instIdy->lpp = (list "prBoundary" "boundary"))))))))))
			
  
(defun daisyCopyBoundaries (@optional (libName  "tcbn40lpbwphvt_110a"))
  (let ((layoutView nil) 
	(symTermList (list )) (currentLib nil) (hitMe nil)
	(vpwrPort nil) (vgndPort nil) (vddlPort nil) (aList nil))
    
    (foreach lib (ddGetLibList)
	     (if (equal libName lib->name) 
		 (setq currentLib lib)))
    (foreach cell  currentLib~>cells
	     (setq layoutView (dbOpenCellViewByType cell->lib->name cell->name "layout" nil "a"))	     
	     (if layoutView 
		 (progn 
		   (daisyCopyBoundary layoutView)
		   (dbSave layoutView)
		   (dbClose layoutView))))))

(defun daisyTransformCoords (rot xPos yPos shape @optional (boundary t) )
  ;; Transform coordinates within an instance to coordinates in the current cell view
  (let ( ( cpos (list 0 0) ) (dim (list 0 0)) (localShape nil) (pos (list 0 0 )))
    
    (setq localShape (if boundary (daisyBoundaryBox shape) shape->bBox))

    (case rot
	  ("R0"	
	   (setq pos  (list xPos+(caar localShape) yPos+(cadar localShape) 
			    xPos+(caadr localShape) yPos+(cadadr localShape)) ))
	  ("R90"	
	   (setq pos  (list xPos-(cadar localShape) yPos+(caar localShape) 
			    xPos-(cadadr localShape) yPos+(caadr localShape)) ))
	  ("R180"	
	   (setq pos  (list xPos-(caar localShape) yPos-(cadar localShape) 
			    xPos-(caadr localShape) yPos-(cadadr localShape)) ))
	  ("R270"	
	   (setq pos  (list xPos+(cadar localShape) yPos-(caar localShape) 
			    xPos+(cadadr localShape) yPos-(caadr localShape)) )))
    
    ;; Find the center point

    (setq cpos  (list ((max (car pos) (caddr pos))-(min (car pos) (caddr pos)))/2+(min (car pos) (caddr pos))
		      ((max (cadr  pos) (cadddr pos))-(min (cadr  pos) (cadddr pos)))/2+(min (cadr  pos) (cadddr pos))))
    
    ;; Find the size
    (setq dim   (list (max (car pos) (caddr pos))-(min (car pos) (caddr pos)) 
		      (max (cadr  pos) (cadddr pos))-(min (cadr  pos) (cadddr pos))))
    
    (if (equal shape->objType "label") 	
	(progn 	  
	  (case rot
		("R0"	
		 (setq pos  (list xPos+(car shape->xy) yPos+(cadr shape->xy))) )
		("R90"	
		 (setq pos  (list xPos-(cadr shape->xy) yPos+(car shape->xy) )))
		("R180"	
		 (setq pos  (list xPos-(car shape->xy) yPos-(cadr shape->xy) )))
		("R270"	
		 (setq pos  (list xPos+(cadr shape->xy) yPos-(car shape->xy) ))))
	  (setq cpos pos)
	  ;; Find the size
	  (setq dim (list 0 0))))
    (list cpos dim)))

(defun daisyBoundaryBox (cellMaster)
  (let ((subBox nil))
    (foreach subShape cellMaster->shapes  
	     (if (equal "rect" subShape->objType) 
		 (if 
		     (member (car subShape->lpp) (list "prBoundary" "instance"))
		     ;; (equal "instance" (car subShape->lpp)) )
		     (setq subBox subShape->bBox))))
    (if subBox subBox cellMaster->bBox))
  )

(defun getInstanceSpace (oldPos rot newCell oldCell @optional (boundary t) (test nil))  
  ;; Compute the required spacing between the center of two cells
  (let ((newWidth 0) 
	(oldWidth 0) 
	(distance 0)
	(xPos (car oldPos)) 
	(yPos (cadr oldPos))
	(newCellBox (list (list 0 0) (list 90 160)))
	(oldCellBox (list (list 0 0) (list 90 160))) )

    (if boundary
	(progn
	  (printf "Boundary: %L \n " (setq newCellBox (daisyBoundaryBox newCell)))
	  (setq oldCellBox (daisyBoundaryBox oldCell)))
      (progn
	(setq newCellBox newCell->bBox)
	(setq oldCellBox oldCell->bBox)))
    
    (setq xPos (car oldPos))
    (setq yPos (cadr oldPos))
    
    (setq newWidth  (abs (caar newCellBox) - (caadr newCellBox)))
    (setq oldWidth (if (equal oldCell nil)
		       0
		     (abs (caar oldCellBox) - (caadr oldCellBox))))
    
    (printf "======= :: %L / %L \n" (list newWidth) (list oldWidth))
    (setq distance oldWidth) ;; (plus newWidth oldWidth)/2)
    (case rot
	  ("R90"  (setq yPos yPos+distance))
	  ("R270" (setq yPos yPos-distance))
	  ("R0"   (setq xPos xPos+distance))
	  ("R180" (setq xPos xPos-distance )))
    (list xPos yPos)))


(defun daisySwapLocations (@optional (object1 nil) (object2 nil))
  (let ((twoObjects nil) (loc nil) (loc nil))
    (if (not object1)
	(progn
	  (setq object1 (car (geGetSelectedSet)))
	  (setq object2 (cadr (geGetSelectedSet)))))
    (setq loc object1->xy)
    (setq rot object1->orient)
    object1->xy = object2->xy
    object1->orient = object2->orient
    object2->xy  = loc
    object2->orient = rot))


(defun daisyTogglePolyContacts (@optional (instances nil))
  (if (not instances) 
      (setq instances (geGetSelSet)))
  
  (if (not (listp instances))
      (setq instances (list instances)))
  
  (foreach inst instances
	   (if (equal inst->libName "tsmcN80")
	       (if (member "polyContacts" inst->prop~>name) 
		   (if (equal inst->polyContacts "FALSE")
		       inst->polyContacts = t
		       inst->polyContacts = nil)
		 (dbCreateProp inst "polyContacts" "boolean" t)))))


(defun daisyTogglePolyRoute (@optional (instances nil))
  (let ((routeList (list "None" "Top" "Bottom" "Both")))
    (if (not instances) 
	(setq instances (geGetSelSet)))
    
    (if (not (listp instances))
	(setq instances (list instances)))
    
    (foreach inst instances
	     (if (equal inst->libName "tsmcN80")
		 (if (member "routePolydir" inst->prop~>name) 
		     (case inst->routePolydir
			   ("None" inst->routePolydir="Top")
			   ("Top" inst->routePolydir="Bottom")
			   ("Bottom" inst->routePolydir="Both")
			   ("Both" inst->routePolydir="None"))
		   (dbCreateProp inst "routePolydir" "string" "Top"))))))


(defun daisyToggleDFMAnalog (@optional (instances nil))
  (let ((routeList (list "None" "Top" "Bottom" "Both")))
    (if (not instances) 
	(setq instances (geGetSelSet)))
    
    (if (not (listp instances))
	(setq instances (list instances)))
    
    (foreach inst instances
	     (if (equal inst->libName "tsmcN80")
		 (if (member "DFM_options" inst->prop~>name) 
		     (case inst->DFM_options
			   ("minRule" inst->DFM_options="DFM")
			   ("DFM" inst->DFM_options="DFM+Analog")
			   ("DFM+Analog" inst->DFM_options="custom")
			   ("custom" inst->DFM_options="minRule"))
		   (dbCreateProp inst "DFM_options" "string" "minRule"))))))


(defun daisyAddLabel ()
  (let ((objects (geGetSelSet)))
    (foreach object objects	     
	     (if (equal object->purpose "pin")
		 (if object->net
		     (daisyLeCreateLabel
		      (hiGetCurrentWindow)->cellView		      
		      object->layerName
		      (list 0.5*(caar object->bBox)+0.5*(caadr object->bBox)
			    0.5*(cadadr object->bBox)+0.5*(cadar object->bBox))
		      object->net->name
		      0.2
		      object))))))


(defun daisyPrintPins ()
  
  (let (( outout nil ) (csvFileName "") (cv (hiGetCurrentWindow)->cellView) (yOff 0) (xOff 0))
    
    (if (boundp 'ENCYOFF) (setq yOff ENCYOFF))
    (if (boundp 'ENCXOFF) (setq xOff ENCXOFF))
    
    
    
    (setq baseFileName (strcat (getShellEnvVar "USERAREA") "/log/" 
			      (makeTempFileName cv->cellName )  ))
    (setq csvFileName (strcat baseFileName ".csv"))
    (setq lefFileName (strcat baseFileName ".lef"))
    
    (setq csvPort (outfile csvFileName))
    (setq lefPort (outfile lefFileName))
    
    (fprintf csvPort "Pinname, Bottom left X, Bottom left Y , Top right X, Top right Y\n")
    
    (foreach shape cv->shapes
	     a=shape
	     (if (and 
		  (equal  (getq shape purpose) "pin")
		  (equal  (getq shape objType) "rect"))
		 (progn 
		   (fprintf csvPort "%s,%f,%f,%f,%f,%f,%f,%f,%f\n"
			    shape->net->name
			    (float (caar shape->bBox))
			    (float (cadar shape->bBox))
			    (float (caadr shape->bBox))
			    (float (cadadr shape->bBox))
			    (float xOff+(caar shape->bBox))
			    (float yOff+(cadar shape->bBox))
			    (float xOff+(caadr shape->bBox))
			    (float yOff+(cadadr shape->bBox))
			    )
		   (fprintf lefPort "PIN %s\n    DIRECTION %s ;\n    PORT\n    LAYER %s \n    RECT %f %f %f %f ;\n    END\n END %s\n"
			    shape->net->name
			    shape->net->term->direction
			    shape->layerName
			    (float xOff+(caar shape->bBox))
			    (float yOff+(cadar shape->bBox))
			    (float xOff+(caadr shape->bBox))
			    (float yOff+(cadadr shape->bBox))
			    shape->net->name))))
    (close csvPort) (close lefPort)
    (sh (sprintf nil "daisyOpenOoo %s " csvFileName))
    (sh (sprintf nil "emacs -fg black %s &" lefFileName))
    
    ))


(defun daisyCalcArea (bBox)
  (abs (times (caar bBox)-(caadr bBox)
	 (cadar bBox)-(cadadr bBox))))


(defun daisyReportInfo (objectz)
  (let ((selObjects (geGetSelSet)))

    (if objectz
	(setq selObjects objectz))
    
    (if selObjects
	(progn
	  (if (not (listp selObjects))
		(setq selObjects 
		      (list selObjects)))

	  (foreach object  selObjects
		   
		   (printf "Object Info: \n================= \n")
		   (if (equal object->objType "rect")
		       
		       (printf "Area: %3.3f\n" 
			       (daisyCalcArea object->bBox)))

		   )))))

(defun daisyReportPositions ()
  (let (( cv nil) (chils nil) )
    (setq cv (hiGetCurrentWindow)->cellView)
    (setq lefFileName (strcat cv->cellName ".lef"))
    (setq lefPort (outfile lefFileName))
    (fprintf lefPort "COMPONENTS %d ;\n" (length cv->instances))
    (foreach inst cv->instances
	     (fprintf lefPort 
		      "   - %s  %s + <FIXED> %L %s  \n" 
		      inst->baseName 
		      inst->cellName
		      inst->xy
		      inst->orient))
    (fprintf lefPort "END COMPONENTS\n")
    (close lefPort)))

(defun daisyPinny (@optional (pinPair nil) (cv nil))
;; Create a net
  (let ((thePin nil) 
	(theNet nil)
	(theLabel nil))    
    (foreach obj (if pinPair pinPair (geGetSelSet))
	     (if (equal obj->objType "label")
		 (setq theLabel obj))
	     (if (equal obj->objType "rect")
		 (setq thePin obj)))
    (setq theNet 
	  (dbMakeNet (if (not cv) (hiGetCurrentWindow)->cellView  cv)
		     theLabel->theLabel))
    (printf "The Label is %s.\n" theNet->name)
    (dbCreatePin theNet thePin)
    (leAttachFig theLabel thePin)
    ))

		 
(defun daisyPinThemDown (selPinLblSet @optional (cv nil))
  (let (	
	(pinSet         nil) 
	(lblSet         nil) 
	(pinId nil)
	(pinLblPairSet  nil)
	(lblXY         (list 0 0))	
	)
    
    (if (not cv) (setq cv (hiGetCurrentWindow)->cellView))

    ;; set to only select pin layers
    
    (if (not selPinLblSet)
	(setq selPinLblSet 
	      (cv->shapes)))
    
    (setq pinSet (setof shape selPinLblSet (and (not (equal shape~>layer "text")) (equal shape~>purpose "pin") (equal shape~>objType "rect"))))
    (setq lblSet (setof shape selPinLblSet (and (equal shape~>purpose "pin") (equal shape~>objType "label"))))
    
    (foreach label lblSet
	     
	     (setq lblXY label->xy)
	     (setq lblX (car lblXY))
	     (setq lblY (cadr lblXY))

	     (setq pinId (car 
			  (setof pin pinSet (and (equal pin->purpose label->purpose)
						 (lblX >= (min (caar pin->bBox) (caadr pin->bBox)))
						 (lblX <= (max (caar pin->bBox) (caadr pin->bBox)))
						 (lblY >= (min (cadar pin->bBox) (cadadr pin->bBox)))						 
						 (lblY <= (max (cadar pin->bBox) (cadadr pin->bBox)))))))
	     (setq pinLblPairSet (append (list (list label pinId)) pinLblPairSet))
	     
	     )
    (foreach pinPair pinLblPairSet
	     (printf "%L %s in %s \n" pinPair  (car pinPair)->theLabel cv->cellName )
	     (daisyPinny pinPair cv))))

(defun daisyFillArea (@key (fillBoundary nil)
			   (objectToFill nil)
			   (fillObject   nil)
			   (flipOddX     nil)
			   (flipOddY     nil)   
			   (fillRect     nil)
			   (fillGrid     0.42)
			   )


  (let ( (pId nil) 
	 (lX 0)
	 (lY 0)
	 (uX 0)
	 (uY 0)
	 (cellId nil)
	 (capId nil)
	 (decoupId nil)
	 (capBbox nil)
	 (deltaX 0)
	 (deltaY 0)
	 (numCap 1)
	 (insideId nil)
	 (rectId nil)
	 )
    
    (if (not fillBoundary)
	(setq pId objectToFill) ;; (car (geGetSelSet)))
      (setq pId fillBoundary))
    
    (setq pBox pId->bBox)
    (setq lX (caar pBox))
    (setq lY (cadar pBox))
    (setq uX (caadr pBox))
    (setq uY (cadadr pBox))
    
    (setq cellId (hiGetCurrentWindow)->cellView)
    (setq capId  (hiGetCurrentWindow)->cellView)
    
    (if (not fillObject)
	(setq decoupId
	      (dbOpenCellViewByType 
	       "daisyTime"
	       "daisyTimeDecoupCapUnit"
	       "layout"))
      (setq decoupId fillObject))
    
    (setq capBbox (daisyBoundaryBox decoupId))
    
    (if fillRect 
	(setq capBbox (list (list 0 0) (list fillGrid fillGrid))))

    (setq deltaX (abs (caadr capBbox)-(caar capBbox)))
    (setq deltaY (abs (cadadr capBbox)-(cadar capBbox)))
    
    (if  (and (greaterp deltaY 0.00) 
	  (greaterp deltaX 0.00))
    (progn

      (setq y lY)
      (while (lessp y uY)
	(setq x lX)
	(while (lessp x uX)
	  (setq rectId
		(dbCreateRect cellId "text" 
			      (list 
			       (list x y) 
			       (list x+deltaX y+deltaY))))
	  (setq insideId (dbLayerInside cellId "text" 
					(list rectId)
					(list pId)))
	  (if insideId
	      
	      (progn 
		(foreach obj insideId (dbDeleteObject obj))		
		
		(if (not fillRect)
		    (leAttachFig pId 
				 (dbCreateInst
				  cellId   
				  decoupId
				  (sprintf nil "IautoDecap_%d_" numCap)
				  (list x y)
				  "R0")))
		
		(setq numCap (plus numCap 1))))

	  (if (not (and insideId fillRect))
	      (dbDeleteObject rectId))
	  
	  (setq x x+deltaX))
	(setq y y+deltaY))            
      (printf "Number of placed instances: %d \n " numCap-1)
      )
    )
    numCap-1
    ))

(defun daisyFillAreas (@key
		       (fillBoundary  nil)
		       (objectsToFill nil)
		       (fillObject    nil)
		       (fillRect      nil)
		       (fillGrid      0.42))
  (if (and objectsToFill (not fillBoundary))
      (foreach element objectsToFill
	       (daisyFillArea 
		?fillBoundary nil
		?objectToFill element
		?fillObject fillObject
		?fillRect   fillRect
		?fillGrid fillGrid))
    
    (daisyFillArea 
     ?fillBoundary fillBoundary
     ?objectToFill objectsToFill
     ?fillObject   fillObject
     ?fillRect     fillRect
     ?fillGrid     fillGrid))
  )

(defun daisyPrintArea ()
  (let ((beBox (hiGetCurrentWindow)->cellView->bBox))
    (printf "Cell area: %3.3e sq mm\n" (abs 
			      1e-6*(times 
			       (caar beBox)-(caadr beBox)
			       (cadadr beBox)-(cadar beBox))))))

(defun daisyFillCellArea (ddc)
  (let (
	(libName  (hiGetCurrentWindow)->cellView->libName)
	(cellName (hiGetCurrentWindow)->cellView->cellName)
	(insts (hiGetCurrentWindow)->cellView->instances)
	(isFillCell nil)
	(runCmd 
	 (strcat "fillCellArea " ) )	
	)
    (setq 
     isFillCell
     (setof x insts (equal x->cellName 
			   (strcat 
			    cellName
			    "_DM"))))
    (sprintf runCmd "fillCellArea %s %s %s" 
	     ddc libName cellName 		   
	     )
    (if isFillCell	
	(progn
	  (sprintf runCmd 
		   "%s %3.1f %3.1f %3.1f %3.1f &"
		   runCmd 
		   (caar (car isFillCell)->bBox)-0.6
		   (cadar (car isFillCell)->bBox)-0.6
		   (caadr (car isFillCell)->bBox)+0.6
		   (cadadr (car isFillCell)->bBox)+0.6)	  
	  (printf "Executing : %s\n" runCmd)
	  (sh runCmd)
	  )
      (progn
	
	;;
	;; else check for boundary layer
	;; if no boundary layer -- do not perform fill action.
	;; need to check that libs are defined, etc.
	;;
	;; For future design
	;;
	(printf "Start from scratch ...\n")
	)
      )    
    ;; (sh runCmd))
  ))


(defun daisyPolyCenter (polygonPoints)
  (let ( (minX 999999) (maxX -999999)
	 (minY 999999) ( maxY -999999))

    (foreach polyPoint polygonPoints
	     (if (lessp (car polyPoint) minX)
		 (setq minX (car polyPoint)))
	     (if (greaterp (car polyPoint) maxX)
		 (setq maxX (car polyPoint)))
	     (if (lessp (cadr polyPoint) minY)
		 (setq minY (cadr polyPoint)))
	     (if (greaterp (cadr polyPoint) maxY)
		 (setq maxY (cadr polyPoint)))
	     )
    (list 0.5*(plus maxX minX) 
	  0.5*(plus maxY minY))))

(defun daisyReturnPolyCenters ()
  (let ((cv (hiGetCurrentWindow)->cellView)
	(padCenterPoint (list 0 0))
	(centerList (list )))
    
    (foreach shape cv->shapes
	     
	     (setq padCenterPoint (daisyPolyCenter shape->points))
;;	     (dbCreatePath cv "text" 
;;			   (list 
;;			    (list (car padCenterPoint)-26.0
;;				  (cadr padCenterPoint)-26.0)
;;			    (list (car padCenterPoint)+26
;;				  (cadr padCenterPoint)+26))
;;			   1)
;;	     (dbCreatePath cv "text" 
;;			   (list 
;;			    (list (car padCenterPoint)+26.0
;;				  (cadr padCenterPoint)-26.0)
;;			    (list (car padCenterPoint)-26
;;				  (cadr padCenterPoint)+26))
;;			   1)
	     (setq centerList (append centerList (list padCenterPoint))))
    (printf "%L \n" padCenterPoint)
    centerList))

(defun daisyFindSides (cornerSize chipSize padCoords)
  (let ((cv (hiGetCurrentWindow)->cellView)
	(logPort nil)
	(westLife (list ))
	(eastEnders (list ))
	(southFork (list ))
	(Nort (list ))
	(fEast (list ))
	(goWest (list ))
	(noPad 1))

  ;; 6820 
  ;; 190
  ;; list

  (printf "Length: %4.0f \n" (float (length padCoords)))
  
  (setq westLife 
	(setof x padCoords (lessp (car x) 
				  cornerSize)))
  (setq eastEnders
	(setof x padCoords (greaterp (car x) 
				     (car chipSize)-cornerSize)))
  (setq southFork
	(setof x padCoords (lessp (cadr x) 
				  cornerSize)))
  (setq North 
	(setof x padCoords (greaterp (cadr x) 
				     (cadr chipSize)-cornerSize)))
  
  (printf "Length: %4.0f \n" (float (length westLife)))
  (printf "Length: %4.0f \n" (float (length southFork)))
  (printf "Length: %4.0f \n" (float (length eastEnders)))
  (printf "Length: %4.0f \n" (float (length North)))

  (setq goWest (list ))
  (foreach element westLife
	   (setq goWest (append goWest (list (reverse element)))))

  (setq fEast (list ))
  (foreach element eastEnders
	   (setq fEast (append fEast (list (reverse element)))))

  (setq goWest (sortcar goWest 'greaterp))
  (setq southFork (sortcar southFork 'lessp))
  (setq fEast (sortcar fEast 'lessp))
  (setq North (sortcar North 'greaterp))

  (setq logPort  (outfile (strcat 
			   (strcat 
			    "./log/" 
			    cv->cellName
			    ".pinlog.csv"))))
  (foreach element goWest
	   (dbCreateLabel (hiGetCurrentWindow)->cellView
			  "text" (reverse element)
			  (sprintf nil "%d" noPad)
			  "centerCenter" "R0" "stick" 12)
	   (fprintf logPort 
		    (sprintf nil "%d, %f, %f\n"
			     noPad 
			     (float (car (reverse element)))
			     (float (cadr (reverse element)))))
	   (setq noPad noPad+1)
	   
	   )
			  
  (foreach element southFork
	   (dbCreateLabel (hiGetCurrentWindow)->cellView
			  "text" element
			  (sprintf nil "%d" noPad)
			  "centerCenter" "R0" "stick" 12)
	   (fprintf logPort 
		    (sprintf nil "%d, %f, %f\n"
			     noPad 
			     (float (car  element))
			     (float (cadr  element))))
	   (setq noPad noPad+1))

  (foreach element fEast
	   (dbCreateLabel (hiGetCurrentWindow)->cellView
			  "text" (reverse element)
			  (sprintf nil "%d" noPad)
			  "centerCenter" "R0" "stick" 12)
	   (fprintf logPort 
		    (sprintf nil "%d, %f, %f\n"
			     noPad 
			     (float (car (reverse element)))
			     (float (cadr (reverse element)))))
	   (setq noPad noPad+1))
			  
  (foreach element North
	   (dbCreateLabel (hiGetCurrentWindow)->cellView
			  "text" element
			  (sprintf nil "%d" noPad)
			  "centerCenter" "R0" "stick" 12)
	   (fprintf logPort 
		    (sprintf nil "%d, %f, %f\n"
			     noPad 
			     (float (car element))
			     (float (cadr  element))))
	   (setq noPad noPad+1))

  (close logPort)
  )
)
  
;; (daisyFindSides 190 (list 3445 4515) (daisyReturnPolyCenters))
(defun daisyLeCreateHilite ()

  (let (       ( cw (hiGetCurrentWindow) ) 
	       ( cv (hiGetCurrentWindow)->cellView )
	       ( xPos 0.0)
	       ( yPos 0.0)
	       ( setA nil)
	       ( cirB nil)
	       )
    ;;
    ;;geDeleteAllHilightSet(cv)
    (setq xPos (times 0.001 (car (hiGetDbuPoint cw))))
    (setq yPos (times 0.001 (cadr (hiGetDbuPoint cw))))    
    (setq setA (geCreateHilightSet cv (list "hilite" "drawing1")))
    (setq cirB (geAddHilightCircle setA (list xPos yPos) 20.0 * 2))
    (setq rectB (geAddHilightRectangle setA (list (list xPos-20.0 yPos-20.0) (list xPos+20.0 yPos+20.0))))
    (setq labB (geAddHilightLabel setA (list xPos yPos) "DRC" "lowerLeft" "R0" "stick" 2 nil nil))
    (setq linB1 (geAddHilightLine setA (list (list xPos-1 yPos) (list xPos+1 yPos))))
    (setq linB1 (geAddHilightLine setA (list (list xPos yPos-1) (list xPos yPos+1))))

    (gePopHilightStack cv)
    setA->enable = t ))

;geDeleteAllHilightSet(cv)

(defun daisyPinToLabel ()
  (let ((cv (hiGetCurrentWindow)->cellView))    

    (foreach pin (setof x cv->shapes x->pin)
	     (leAttachFig 	      
	      (dbCreateLabel 
	       cv
	       (list "PIN"
		     (cadr 
		      (assoc 
		       pin->layerName 
		       (list (list "MET1" "metal1")
			     (list "MET2" "metal2")
			     (list "MET3" "metal3")
			     (list "MET4" "metal4")))))
	       (daisyCenterPoint pin->bBox 0.025)
	       pin->pin->term->name			     
	       "centerCenter"  
	       "R0" "stick" 0.2)	      
	      pin))))


