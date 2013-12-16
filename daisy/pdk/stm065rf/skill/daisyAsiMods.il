;;; ----------------------
;;; Daisy 
;;; ----------------------

(printf "DAISY:: ... loading pdk-specific daisyAsiMods.il ... \n")
(printf "DAISY:: ... in order to override the default settings ... \n")

(defun daisyPresentRegionsPopUp ()
		  
  ;; Works for all transistors in cmos065RF DK 522 , tested in Cadence 5.1.41
  ;; Devices from cmos065, cmos065_a, cmos065_hv, cmos065_rf are supported
  ;; Used for short-cut key. Uses a file to temporarily store the 
  ;; data and then display it in a pop-up window.

  ;; Could be updated to return a list of components, their path, and their operation  regions, margins. 
  ;; Would be useful for plotting results.
  (printf "STM065 Specific PresentRegionsPopUp Function\n")

  (let ( (selectedTransistors nil)
	 (operationRegion nil)
	 (operationMargin 0)
	 (selectedObjects (geGetSelectedSet))
	 (l_transistors  
	  (list  "nhvtgp" "nhvtgprpo" "nhvtlp" "nhvtlprpo" "nlvtgp" "nlvtlp" "nsvtgp" "nsvtgprpo" "nsvtlp" "nsvtlprpo"
			       "phvtgp" "phvtgprpo" "phvtlp" "phvtlprpo" "plvtgp" "plvtlp" "psvtgp" "psvtgprpo" "psvtlp" "psvtlprpo"
			       "nnvt18" "nsvt18" "nsvt18rpo" "nsvt18rponoldd" "psvt18" "psvt18rpo"
			       "ndr25" "nnvt25" "nsvt25" "nsvt25rpo" "nsvt25rponoldd" "pdr25" "psvt25" "psvt25rpo"
			       "nhpalp" "phpalp"
			       "next18hv" "pext18hv"
			       "nhpalp_rf" "nlvtlp_rf" "nsvt18_rf" "nsvtlp_rf"  
			       "phpalp_rf" "plvtlp_rf" "psvt18_rf" "psvtlp_rf"
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
			   (rexCompile "/")
			   (setq transName (rexReplace transName "" 1))
			   (rexCompile "/")
			   (setq transName (rexReplace transName "." 0))

			   (if (member singleObject->cellName l_transistors)
			       (setq transName (strcat transName ".m1")))

			   (rexCompile "<")
			   (setq mathExp  (strcat "(pv \"" transName  "\" \"region\" ?result \"dcOpInfo-info\")" ))
 			   (setq mathExp (rexReplace mathExp "_" 0))
			   (setq mathExp2 (strcat "(pv \"" transName  "\" \"vds\"    ?result \"dcOpInfo-info\")" ))
			   (setq mathExp2 (rexReplace mathExp2 "_" 0))
			   (setq mathExp3 (strcat "(pv \"" transName  "\" \"vgs\"    ?result \"dcOpInfo-info\") - (pv \"" transName  "\" \"vth\"    ?result \"dcOpInfo-info\") "   ))
			   (setq mathExp3 (rexReplace mathExp3 "_" 0))
			   (rexCompile ">")
			   (setq mathExp  (rexReplace mathExp  "" 0))
			   (setq mathExp2 (rexReplace mathExp2 "" 0))
			   (setq mathExp3 (rexReplace mathExp3 "" 0))
			   
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
				    (rexCompile "/")
				    (setq transName (rexReplace transName "" 1))

				    (rexCompile "/")
				    (setq transName (rexReplace transName "." 0))

				    (setq transName (sprintf nil "%s_%d" transName instNo))
				    
				   
					(setq transName (strcat transName ".m1"))
					
				    (rexCompile "<")
				    (setq mathExp  (strcat "(pv \"" transName  "\" \"region\" ?result \"dcOpInfo-info\")" ))
				    (setq mathExp (rexReplace mathExp "_" 0))
				    (setq mathExp2 (strcat "(pv \"" transName  "\" \"vds\"    ?result \"dcOpInfo-info\")" ))
				    (setq mathExp2 (rexReplace mathExp2 "_" 0))
				    (setq mathExp3 (strcat "(pv \"" transName  "\" \"vgs\"    ?result \"dcOpInfo-info\") - (pv \"" transName  "\" \"vth\"    ?result \"dcOpInfo-info\") "   ))
				    (setq mathExp3 (rexReplace mathExp3 "_" 0))
				    (rexCompile ">")
				    (setq mathExp  (rexReplace mathExp  "" 0))
				    (setq mathExp2 (rexReplace mathExp2 "" 0))
				    (setq mathExp3 (rexReplace mathExp3 "" 0))
				    (setq operationRegion (evalstring mathExp))	
				    (printf "%s\n" mathExp)
				    (printf "%s\n" mathExp2)
				    (printf "%s\n" mathExp3)

				    (if operationRegion then 
				      (setq operationRegion (nth (evalstring mathExp) l_regionLookUp))
				      (setq operationMargin (if (equal "p" (substring singleObject~>cellName 1 1)) 
							      -(evalstring mathExp2)+(evalstring mathExp3)
							      (evalstring mathExp2)-(evalstring mathExp3)))
				      (setq selectedTransistors (cons singleObject~>name selectedTransistors ))
	;;			      (printf "jwejje")
				      (fprintf oport "%32s   : %s   %2.2f V %s\n" 
					       transName operationRegion 
					       operationMargin
					       (if (lessp operationMargin 0.1) "oops!" ""))

				      else	   
				      (fprintf oport "%32s    :        %s \n" 
					       transName "Undefined!"))))))
	      (fprintf oport  "\n===============================================================\n")
	      (close oport)
	      (view fileName nil (strcat "Operation Regions -- " (getCurrentTime) " -- " (getCurrentWindow)-> cellView -> cellName))
	      (deleteFile fileName))
	  (error "Couldn't open output file")))) ))


;; Skill script to report NMOS devices whose bulks are not connected to ground node
;; get list of nmos instances in the schematic (supports nmos from ST 65nm)
(defun daisyCheckBulkConnNMOS ()	
(setq instList nil)
(foreach inst (setof x (hiGetCurrentWindow)->cellView->instances
 (member x->cellName (list "nlvtlp" "nsvtlp" "nhvtlp" "nlvtgp" "nsvtgp" "nhvtgp")))
  (printf "%s %s \n" inst->libName inst->cellName)
(setq instList (append (list inst) instList))
)
;; get list of terminal of each nmos (each nmos has 4 terminals)
;; order of terminals is B,S,G,D
(setq instConnsList nil)
(foreach inst instList
(setq instConnsList (append (list inst->conns) instConnsList))
)

(foreach instConnsElem instConnsList
 inst_B = nth(0 instConnsElem)
 inst_S = nth(1 instConnsElem)
inst_B_sigNode = car(inst_B->net->sigNames)
inst_S_sigNode = inst_S->net->sigNames
(if (not (member inst_B_sigNode (list "Gnd" "gnd" "vss" "vgnd")))
( progn
(printf "ATTENTION!\n")
(printf "Terminal %s of device %s is connected to signal node %s !!! \n" inst_B->name inst_B->inst->baseName  inst_B_sigNode)
)
)
)
)


