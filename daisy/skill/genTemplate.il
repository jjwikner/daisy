(printf " ... loading %s.il ...\n" "genTemplate")

;; Change path!
(defun ltf ()
  (foreach file (list "skill")
	   (loadi (strcat (getShellEnvVar "USERAREA") "/skill/"
			  (sprintf nil "%s.il" file)))))


(setq deleteAll t)

;, Change workLibName and cellView!
(setq workLibName "daisyADC")
(setq cellView    "daisyAdcDlyLine")


(setq cellId (dbOpenCellViewByType 
	      workLibName cellView 
	      "layout" "maskLayout" "w"))
(setq winId nil)
(foreach openWinId (hiGetWindowList)
	 (if (equal (getq openWinId cellView) cellId)
	     (setq winId openWinId)))
(if (not winId) (setq winId (hiOpenWindow)))
(geOpen ?window winId
	?lib workLibName
	?cell cellView
	?view "layout"
	?viewType "maskLayout"
	?mode "a")
(if deleteAll 
    (progn
      (geSelectAllFig cellId)
      (if (geGetSelectedSet)
	  (leHiDelete winId))
      (dbDeleteAllNet cellId)))





;; Useful Commands
;; (setq nandId    (dbOpenCellViewByType workLibName "cellName" "layout"))
;; (daisyAttachFig (dbCreateInst cellId nandId "instName" (list xPos yPos) "R0") figId)
;; (daisyCreatePin cellId ?position (list xPos yPos) ?pinLayer "pinLayer" ?dimensions (list 0.2 0.24) ?pinName "pinName" ?pinDirection "inputOutput" ?masterId figId)
;; (daisyCreateContact cellId (list "cont1" "cont2" ...) (list xPos yPos) contLength contWidth  figId "netName?")
;; (daisyPath cellId "pathLayer" (list (list x1 y1) (list x2 y2)) pathWidth figId (dbMakeNet cellId "netname"))
;; (daisyRect cellId "instance" (list minCorner maxCorner)) ; Add instance layer
	   
