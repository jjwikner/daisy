;;; ----------------------
;;; Daisy 
;;; ----------------------

(printf " ... loading pdk-specific daisyAsiMods.il ... \n")


(defun presentRegionsPopUp ()		  
  ;; Used for short-cut key. Uses a file to temporarily store the 
  ;; data and then display it in a pop-up window.

  ;; Could be updated to return a list of components, their path, and their operation  regions, margins. 
  ;; Would be useful for plotting results.
  (let ( (selectedTransistors nil)
	 (operationRegion nil)
	 (operationMargin 0)
	 (selectedObjects (geGetSelectedSet))
	 (l_transistors  (list "nch" "nch_25" "pch_25" 
			       "P_33_MM" "P_18_MM" "N_33_MM" "N_18_MM"
			       "nsvt25rf" "psvt25rf" "nsvtrf" "psvtrf"
			       "nsvt25" "psvt25" "nsvt" "psvt"
			       "nsvt33" "psvt33"
			       "nhvt25" "phvt25" "nhvt" "phvt"
			       "nlvt25" "plvt25" "nlvt" "plvt"
			       "nch_33" "nch_33_dnw" "nch_33_dnwx" "nch_33x" "nch_dnw" "nch_dnwx" "nch_hvt" 
			       "nch_hvt_dnw" "nch_hvt_dnwx" "nch_hvtx" "nch_lvt" "nch_lvt_dnw" "nch_lvt_dnwx" 
			       "nch_lvtx" "nch_na" "nch_na33" "nch_na33x" "nch_nax" "nchx" "pch" "pch_33" 
			       "pch_33x" "pch_hvt" "pch_hvtx" "pch_lvt" "pch_lvtx" "pchx" ))
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
	      (fprintf oport " Cell Name       Transistor      Operation region        Margin  \n")
	      (fprintf oport "---------------------------------------------------------------  \n\n")
	      
	      (foreach singleObject selectedObjects 
		       (when (member singleObject~>cellName l_transistors)
			 
			 (setq hierPath (geGetInstHier))
			 (if (equal hierPath "/") (setq hierPath "") )
			 
			 (if (equal singleObject->numInst 1) then
			   (setq mathExp  (strcat "(pv \"" hierPath  singleObject~>name  ".M1\" \"region\" ?result \"element-info\")" ))
			   (setq mathExp2 (strcat "(pv \"" hierPath  singleObject~>name  ".M1\" \"vds\"    ?result \"element-info\")" ))
			   (setq mathExp3 (strcat "(pv \"" hierPath  singleObject~>name  ".M1\" \"vdsat\"  ?result \"element-info\")" ))
			   (setq operationRegion (evalstring mathExp))	
			   (printf "%s" mathExp)
			   (if operationRegion then 
			     (setq operationRegion (returnTransistorRegion (evalstring mathExp)))
			     (setq operationMargin (if (equal "p" (substring singleObject~>cellName 1 1)) 
						       -(evalstring mathExp2)+(evalstring mathExp3)
						       (evalstring mathExp2)-(evalstring mathExp3)))
			     (setq selectedTransistors (cons singleObject~>name selectedTransistors ))
			     (fprintf oport "d %10s   %9s   : %10s     %s   %2.2f V  %s \n" 
				      hierPath singleObject~>name operationRegion 
				      (if (lessp operationMargin 0) "" " ") operationMargin 
				      (if (lessp operationMargin 0.1) "oops!" ""))
			     
			     else	   
			     (fprintf oport "%10s   %9s   : %10s        %2.2f V \n" 
				      hierPath singleObject~>name "Undefined!" (float 0)))
			   else
			   ;; Iterated instance
			   (printf "Iterated instance. Assuming <0:%d> or <%d:0>.\n" singleObject->numInst-1 singleObject->numInst-1)
			   ;; Not needed, use singleObject -> baseName instead
			   (setq subStrLen 1)
			   (while (and 
				   (not (rexMatchp "<" (substring singleObject~>name 1 subStrLen))) 
				   (lessp subStrLen (plus 1 (strlen singleObject~>name))))
			     (setq subStrLen (plus subStrLen 1)))
			   (setq objNameStripped (substring singleObject~>name 1 (subStrLen - 1)))
			   (printf "%s  \n" objNameStripped)
			   ;; Ugly, but doable.
			   (setq numbStr  (substring singleObject~>name (strlen objNameStripped)+1 (strlen singleObject~>name)-(strlen objNameStripped)))
			   (printf "%s \n" numbStr)
			   (foreach instNo (parseIteratedInstance numbStr)
				    (setq mathExp 
					  (strcat "(pv \"" hierPath singleObject->baseName 
						  (sprintf nil "<%d>" instNo)  ".M1\" \"region\" ?result \"element-info\")" ))
				    (setq mathExp2 (strcat "(pv \"" hierPath  singleObject~>baseName (sprintf nil "<%d>" instNo)  ".M1\" \"vds\" ?result \"element-info\")" ))
				    (setq mathExp3 (strcat "(pv \"" hierPath  singleObject~>baseName (sprintf nil "<%d>" instNo) ".M1\" \"vdsat\" ?result \"element-info\")" ))
				    (setq operationMargin (if (equal "p" (substring singleObject~>cellName 1 1)) 
							      -(evalstring mathExp2)+(evalstring mathExp3)
							      (evalstring mathExp2)-(evalstring mathExp3)))
				    
				    (setq operationRegion (evalstring mathExp))	
				    (if operationRegion then 
				      (setq operationRegion (nth (evalstring mathExp) l_regionLookUp))
				      (setq selectedTransistors (cons singleObject~>name selectedTransistors ))
				      (fprintf oport "%10s   %9s   : %9s%4s     %s   %2.2f V %s\n" 
					       hierPath singleObject~>name (sprintf nil "<%d>" instNo) operationRegion 
					       (if (lessp operationMargin 0) "" " ") operationMargin
					       (if (lessp operationMargin 0.1) "oops!" ""))

				      else	   
				      (fprintf oport "%10s     %9s%4s :        %s \n" 
					       hierPath singleObject->baseName (sprintf nil "<%d>" instNo) "Undefined!"))))))
	      (fprintf oport  "\n===============================================================\n")
	      (close oport)
	      (view fileName nil (strcat "Operation Regions -- " (getCurrentTime) " -- " (getCurrentWindow)-> cellView -> cellName))
	      (deleteFile fileName))
	  (error "Couldn't open output file")))) ))

