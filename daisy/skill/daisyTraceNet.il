(printf " ... loading %s.il ... \n " "daisyTraceNets")

(defun daisyTraceNet (pinName cellId trace @optional ( stopLibList (list "tsmcN80" "analogLib" "basic")) )
  (let ( ( cv nil ) 
	 (netId nil )
	 (uniqueInstList nil ) )

    ;; Add checker to see if cellview exists or not.
    (if (not (member cellId->libName stopLibList))
	cv = (dbOpenCellViewByType 
	      cellId->libName  
	      cellId->cellName 
	      "schematic")
	cv = nil
	)

    (if cv
	(progn
	  netId = (car (setof x cv->nets (equal x->name pinName)))    
	  ;; This can be shortened by removing the 
	  ;; uniqueInstList concept... add the daisyTraceNet 
	  ;; in the first foreach loop instead.
	  (foreach connTerm netId->instTerms
		   (if (not (member connTerm->inst->master uniqueInstList))
		       (setq uniqueInstList 
			     (append uniqueInstList 
				     (list (list connTerm->name connTerm->inst->master connTerm->inst->baseName))))))
	  ;; (printf "%s : %L \n" cv->cellName uniqueInstList)

	  (foreach pPair uniqueInstList
		   (if (cadr pPair) 
		       (progn
			 (daisyTraceNet 
			  (car pPair) 
			  (cadr pPair)
			  (strcat trace "/" (caddr pPair)))
    			 (if (equal (car pPair) "G") 
			     (printf "%s %s  %s %s %s\n" 
				     "Gate connection" 
				     (strcat trace "/" (caddr pPair))
				     cv->libName 
				     cv->cellName 
				     "schematic")) )))
	  ))
			 

))


cv = (dbOpenCellViewByType "daisyAFE" "daisyAFETop" "schematic")
netNames = (list "vpwr10_dig" "vgnd_dig" "VSSAFEGOLD" 
		 "vpwr33Green" "vpwr10Green" "vgndGreen"
		 "vpwr33Aux" "vpwr10Aux" "vgndAux"
		 "vpwr33Blue" "vpwr10Blue" "vgndBlue"
		 "vpwr33Red" "vpwr10Red" "vgndRed"
		 "vpwr33Ref" "vpwr10Reg" "vgndRef"
		 "vgndTime" "vpwr33Time" "vpwr10Time"



)

(setq uniqueInstList nil)

(foreach netName netNames
	 netIds = (car (setof x cv->nets (equal x->name netName)))
	 (foreach connTerm  netIds~>instTerms
		  (if (not (member connTerm->inst->master uniqueInstList))
		      (setq uniqueInstList 
			    (append uniqueInstList 
				    (list (list connTerm->name connTerm->inst->master connTerm->inst->baseName)))))))

 (foreach pPair uniqueInstList
		   (if (cadr pPair) 
		       (daisyTraceNet (defun daisyTraceNet (pinName cellId trace @optional ( stopLibList (list "tsmcN80" "analogLib" "basic")) )
  (let ( ( cv nil ) 
	 (netId nil )
	 (uniqueInstList nil ) )

    ;; Add checker to see if cellview exists or not.
    (if (not (member cellId->libName stopLibList))
	cv = (dbOpenCellViewByType 
	      cellId->libName  
	      cellId->cellName 
	      "schematic")
	cv = nil
	)

    (if cv
	(progn
	  netId = (car (setof x cv->nets (equal x->name pinName)))    
	  ;; This can be shortened by removing the 
	  ;; uniqueInstList concept... add the daisyTraceNet 
	  ;; in the first foreach loop instead.
	  (foreach connTerm netId->instTerms
		   (if (not (member connTerm->inst->master uniqueInstList))
		       (setq uniqueInstList 
			     (append uniqueInstList 
				     (list (list connTerm->name connTerm->inst->master connTerm->inst->baseName))))))
	  ;; (printf "%s : %L \n" cv->cellName uniqueInstList)

	  (foreach pPair uniqueInstList
		   (if (cadr pPair) 
		       (progn
			 (daisyTraceNet 
			  (car pPair) 
			  (cadr pPair)
			  (strcat trace "/" (caddr pPair)))
    			 (if (equal (car pPair) "G") 
			     (printf "%s %s  %s %s %s\n" 
				     "Gate connection" 
				     (strcat trace "/" (caddr pPair))
				     cv->libName 
				     cv->cellName 
				     "schematic")) )))
	  ))
			 

))


cv = (dbOpenCellViewByType "daisyAFE" "daisyAFETop" "schematic")
netNames = (list "vpwr10_dig" "vgnd_dig" "VSSAFEGOLD" 
		 "vpwr33Green" "vpwr10Green" "vgndGreen"
		 "vpwr33Aux" "vpwr10Aux" "vgndAux"
		 "vpwr33Blue" "vpwr10Blue" "vgndBlue"
		 "vpwr33Red" "vpwr10Red" "vgndRed"
		 "vpwr33Ref" "vpwr10Reg" "vgndRef"
		 "vgndTime" "vpwr33Time" "vpwr10Time"



)

(setq uniqueInstList nil)

(foreach netName netNames
	 netIds = (car (setof x cv->nets (equal x->name netName)))
	 (foreach connTerm  netIds~>instTerms
		  (if (not (member connTerm->inst->master uniqueInstList))
		      (setq uniqueInstList 
			    (append uniqueInstList 
				    (list (list connTerm->name connTerm->inst->master connTerm->inst->baseName)))))))

 (foreach pPair uniqueInstList
		   (if (cadr pPair) 
		       (daisyTraceNet 
			(car pPair) 
			(cadr pPair)
			(caddr pPair) ) ) )

			(car pPair) 
			(cadr pPair)
			(caddr pPair) ) ) )
