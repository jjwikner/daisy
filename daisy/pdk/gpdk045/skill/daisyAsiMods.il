;;; ----------------------
;;; Daisy 
;;; ----------------------

(printf "DAISY:: ... loading pdk-specific daisyAsiMods.il ... \n")
(printf "DAISY:: ... in order to override the default settings ... \n")

(defun daisyPresentRegionsPopUp ()

  ;; Used for short-cut key. Uses a file to temporarily store the 
  ;; data and then display it in a pop-up window.

  ;; Could be updated to return a list of components, their path, and their operation  regions, margins. 
  ;; Would be useful for plotting results.
  ;; Modified version of daisyPresentRegionsPopUp() to support mosfets in gpdk045 only

  (let ( (selectedTransistors nil)
	 (operationRegion nil)
	 (operationMargin 0)
	 (selectedObjects (geGetSelectedSet))
	 (l_transistors  
	  (list "nmos1v" "pmos1v" "nmos1v_lvt" "pmos1v_lvt"
		"nmos1v_3" "pmos1v_3" "nmos1v_lvt_3" "pmos1v_lvt_3"
		"nmos1v_hvt" "pmos1v_hvt" "nmos1v_hvt_3" "pmos1v_hvt_3"
		"nmos1v_nat" "nmos1v_nat_3" "nmos2v_nat" "nmos2v_nat_3"
		"nmos2v" "pmos2v" "nmos2v_3" "pmos2v_3"
		 ))
	 (l_regionLookUp (list "Cut-off" "Linear" "Saturation" "Subthreshold" "Break-down")) 
	 (oport nil) 
	 (hierPath "") 
	 (mathExp "") 
	 (objNameStripped nil) )
    
    (if selectedObjects then
      (progn
	(setq oport (outfile (setq fileName (makeTempFileName "/tmp/operationRegions"))))
	(if oport
	    (progn
	      
	      (fprintf oport "\n")
	      (fprintf oport "===============================================================         \n")
	      (fprintf oport "DC simulation results from %s\n" (getCurrentWindow)-> cellView -> cellName)
	      (fprintf oport "List of transistors and their operation regions and margins.     \n")
	      (fprintf oport "===============================================================   \n")
	      (fprintf oport " (Hierarchical) Transistor      Operation region        Margin  \n")
	      (fprintf oport "---------------------------------------------------------------  \n\n")
	      
	      (foreach singleObject selectedObjects 
		       (when (member singleObject~>cellName l_transistors)			 
			 (setq hierPath (geGetInstHier))
			 (if (equal hierPath "/") (setq hierPath "") )			 
			 (if (equal singleObject->numInst 1) then
			   (setq transName (strcat hierPath  (if (equal "" hierPath) "" "/") singleObject->name))
			    (printf "%s\n" transName)
			   (setq mathExp  (strcat "(pv \"" transName  "\" \"region\" ?result \"dcOpInfo-info\")" ))
			   (setq mathExp2 (strcat "(pv \"" transName  "\" \"vds\"    ?result \"dcOpInfo-info\")" ))
			   (setq mathExp3 (strcat "(pv \"" transName  "\" \"vgs\"    ?result \"dcOpInfo-info\") - (pv \"" transName  "\" \"vth\"    ?result \"dcOpInfo-info\") "   ))			   
			   (setq operationRegion (evalstring mathExp))	
			   (printf "%s\n" mathExp)
			   (printf "%s\n" mathExp2)
			   (printf "%s\n" mathExp3)
			   (if operationRegion then 
			     (setq operationRegion (returnTransistorRegion (evalstring mathExp)))
			     (setq operationMargin (if (equal "p" (substring singleObject~>cellName 1 1)) 
						       -(evalstring mathExp2)+(evalstring mathExp3)
						       (evalstring mathExp2)-(evalstring mathExp3)))
			     (setq selectedTransistors (cons singleObject~>name selectedTransistors ))
			     (fprintf oport "%32s     : %10s     %s   %2.2f V  %s \n" 
				      transName  operationRegion 
				      (if (lessp operationMargin 0) "" " ") operationMargin 
				      (if (lessp operationMargin 0.1) "oops!" ""))
			     
			     else	   
			     (fprintf oport "%32s   %9s   : %10s        %2.2f V \n" 
				      transName "Undefined!" "" (float 0)))
			   else
			   ;; Iterated instance
			   (setq objNameStripped singleObject->baseName)
			   (printf "%s  \n" objNameStripped)
			   (setq numbStr  (substring singleObject~>name (strlen objNameStripped)+1 (strlen singleObject~>name)-(strlen objNameStripped)))
			   (printf "%s \n" numbStr)
			   (foreach instNo (parseIteratedInstance numbStr)
				    
				    (setq transName (strcat hierPath  (if (equal "" hierPath) "" "/") singleObject->baseName))
				    (setq transName (sprintf nil "%s<%d>" transName instNo))
				    (setq mathExp  (strcat "(pv \"" transName  "\" \"region\" ?result \"dcOpInfo-info\")" ))
				    (setq mathExp2 (strcat "(pv \"" transName  "\" \"vds\"    ?result \"dcOpInfo-info\")" ))
				    (setq mathExp3 (strcat "(pv \"" transName  "\" \"vgs\"    ?result \"dcOpInfo-info\") - (pv \"" transName  "\" \"vth\"    ?result \"dcOpInfo-info\") "   ))
				    (setq operationRegion (evalstring mathExp))	
				      (printf "%s\n" mathExp)
				      (printf "%s\n" mathExp2)
				      (printf "%s\n" mathExp3)
				    
				    (setq operationRegion (evalstring mathExp))	
				    (if operationRegion then 
				      (setq operationRegion (nth (evalstring mathExp) l_regionLookUp))
				      (setq operationMargin (if (equal "p" (substring singleObject~>cellName 1 1)) 
							      -(evalstring mathExp2)+(evalstring mathExp3)
							      (evalstring mathExp2)-(evalstring mathExp3)))
				      (setq selectedTransistors (cons singleObject~>name selectedTransistors ))
				      (fprintf oport "%32s     : %10s     %s   %2.2f V  %s \n" 
				      transName  operationRegion 
				      (if (lessp operationMargin 0) "" " ") operationMargin 
				      (if (lessp operationMargin 0.1) "oops!" ""))
				      else
				      (fprintf oport "%32s   %9s   : %10s        %2.2f V \n" 
				      transName "Undefined!" "" (float 0)))))))
	      (fprintf oport  "\n===============================================================\n")
	      (close oport)
	      (view fileName nil (strcat "Operation Regions -- " (getCurrentTime) " -- " (getCurrentWindow)-> cellView -> cellName))
	      (deleteFile fileName))
	  (error "Couldn't open output file")))) ))
