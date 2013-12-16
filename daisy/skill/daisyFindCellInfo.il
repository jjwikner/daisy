
;;; Script for identifying cell ports/names in cadence given a spef file.

(printf " ... loading daisyFindCellInfo.il\n")

(defun daisyFindCellInfo (activeCell expression @optional (specialFlag nil))
  (let ( ( compName "")
	 ( transName "" )
	 ( activeIntCell nil )
	 ( activeTrans nil )
	 ( selectedCell nil )
	 ( netToPortName "" )
	 ( absPath "")
	 ( origTransName "")
	 ( origCompName "")
	 )
    (setq activeIntCell  activeCell)
    
    (foreach component (parseString expression "/:")
	     ;; (printf "%s\n" expression)
	     (if (or (equal (substring component 1 1) "X")
		     (equal (substring component 1 1) "x"))
		 (progn 
		   (setq origCompName (substring component 2 (strlen component)-1))
		   ;; Remove iterated instances...		   
		   (setq compName (car (parseString origCompName "<")))
		   ;;(printf "----> %s \n " compName)
		   ;;(printf "-----> %s \n" absPath)
		   (setq absPath (strcat absPath (if (equal absPath "") "" "/") origCompName))
		   ;(printf "Tjoho: %s\n" compName)	       
		   ;;(printf "hahah : %L\n" activeIntCell~>instances~>baseName)
		   ;;(printf "FUCK %L\n " activeIntCell)
		   ;;(printf "SHIT %L\n " activeIntCell->instances~>baseName)
		   (setq selectedCell (car (setof x activeIntCell->instances (equal compName x->baseName))))
		   ;;(printf "aah %L\n" (setof x activeIntCell->instances (equal compName x->baseName)))
		   ;; (printf "jajjdaj\n")
		   ;; (printf ":::o %L\n" selectedCell~>??)
		   ;;(printf "hehe\n")
		   ;;(printf "%L\n" selectedCell~>cellName)
		   (setq activeIntCell (dbOpenCellViewByType 
					selectedCell->libName 
					selectedCell->cellName 
					"schematic" "" "r"))
		   ;;(printf "hihi\n")
		   ))
	     
	     (if (equal (upperCase (substring component 1 2)) "MM")
		 (progn 
		   (setq origTransName component)
		   (setq transName (substring component 2 (strlen component)-1))
		   ;;(printf "This is a transistor: %s\n" transName)
		   ;; This is a transistor -- time to terminate
		   ;; We have to remove the @ too.
		   (setq transName (car (parseString transName "@")))
		   (setq activeTrans (car (setof x activeIntCell~>instances (equal transName x->name))))
		   ;; (printf "::: %s\n" activeTrans->baseName)
		   ;;(setq activeIntCell (dbOpenCellViewByType activeIntCell->libName activeIntCell->cellName "schematic"))
		   ))
	     
	     (if (or (equal (upperCase component) "G")
		     (equal (upperCase component) "D")
		     (equal (upperCase component) "S") )
		 (progn
		   ;; (printf "This is the port of the transistor: %s " component)
		   
		   (setq netToPortName (car (setof x activeTrans~>conns (equal x->name (upperCase component))))->net->name)
		   ;; (printf "%L\n" activeIntCell->pins)
		   ;; We need to check if this is a port or not (otherwise it is an internal net).
		   (if (member netToPortName activeIntCell~>terminals~>name)
		       (setq absPath (strcat absPath ":" netToPortName " *D " activeIntCell->cellName ))
		     (if specialFlag
			 (setq absPath "")
		       (setq absPath (strcat absPath "/" origTransName ":" component)))))
	       )
	     )
    absPath))


(defun daisyCreateHashTable (activeCell)
  (let (( inPort nil)
	( outPort nil )
	( cellNameX activeCell->cellName )
	( expression "" )
	( absPath "" ))
    
    ;;(printf "%L \n" activeCell~>cellName)
    (setq cellNameX activeCell->cellName)
    ;;(printf "%L \n" cellName)
    (setq inPort (infile (strcat cellNameX ".nets.spef")))
    (setq outPort (outfile (strcat cellNameX ".hash.spef")))    
    (fscanf inPort "%s\n" expression)  
    (while   expression
      ;; (printf "%d\n" (setq counter counter+1))
      ;; (printf "%s \n" expression)
      (if (equal (fscanf inPort "%s\n" expression) 1)
	  (progn
	    (setq absPath (daisyFindCellInfo 
			   activeCell 
			   expression t))
	    (if (not (equal absPath ""))
		(fprintf outPort "%s %s\n" 
			 expression absPath)))
	(setq expression nil)))
    
    (close inPort)
    (close outPort)
    ))

(if nil 
    (progn
      (sh (strcat "pexSpefNets.sh " hiGetCurrentWindow()->cellView->cellName    ))
      (daisyCreateHashTable hiGetCurrentWindow()->cellView)
      (sh (strcat "pexSpefConvert.pl " hiGetCurrentWindow()->cellView->cellName ))
      (sh (strcat "daisy/bin/pexSpefClean.sh " hiGetCurrentWindow()->cellView->cellName ))
      ))

