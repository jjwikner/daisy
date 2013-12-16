;;; ----------------------
;;; Daisy 
;;; ----------------------

(printf " ... loading daisyAsiMods.il ... \n")


(defun daisyReturnHarmonics (netName @optional (order 9) (analysis "pss_fd") )

  (let (( harmonicList (list )))
  (setq netWaveForm 
	(getData netName 
		 ?result analysis))
  
   
  (for m 1 order
              
	(setq harmonic  (harmonic netWaveForm m))
	
	(setq harmonicList (append (list (list m (harmonic netWaveForm m)))
				   harmonicList)))
  harmonicList))

(defun daisyReturnHdList (netName @optional (order 9) (analysis "pss_fd") )

  (let ((fundamental 0) (harmList nil) (hdList nil ))

    (setq harmList (daisyReturnHarmonics netName order analysis))
    fundamental = (dB20 (cadr (assoc 1 harmList)))
    
    (foreach harmo harmList 
	     (setq hdList (append (list (list (car harmo) fundamental-(dB20 (cadr harmo))))
				  hdList)))

    (setq hdList (append (list fundamental) hdList))
    hdList))


(defun daisyJobCheck (job)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Checks if a spectre job is done. 
  ;; job = string containg directory path containing the 
  ;; psf directory of the spectre run.
  ;; Output = t if job is done. Nil otherwise 
  ;; Christer Jansson 2008-10-13
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Expects directory path of psf file
  (let (tmpFile myUnixCommand fileToCheck)
    tmpFile = (strcat job "/psf/tmpCheck1") 
    (deleteFile tmpFile) ; returns nil if no file exists 
    fileToCheck = (strcat job "/psf/spectre.out") 
    (if  isFile(fileToCheck) then 
	 myUnixCommand = (strcat "egrep 'spectre completes|spectre terminated|Segmentation fault|Bus error|Spectre received SIGTERM' " fileToCheck " > " tmpFile) 
	 (csh myUnixCommand) 
	 fileLength(tmpFile) > 10 
	 else
	 ;; job may have crashed
	 nil
	 ) ;if
    ) ;let 
  ) ; procedure 


(defun daisyJobWait (jobListToCheck @key (countLimit 600)) 
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Waits until all the spectre jobs in the list are finnished or timed out.
  ;; jobListToCheck = list of strings containing the directory paths to the 
  ;; jobs. 
  ;; Christer Jansson 2008-10-13
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let (myKeepWaiting tmpFile fileToCheck myUnixCommand waited timeOutCount )
    (foreach job jobListToCheck 
	     myKeepWaiting = t 
	     waited = nil 
	     ;; countLimit = 600 ;; About half a minute per count. 120 counts >~ 1h 
	     timeOutCount = 1 
	     (while  (myKeepWaiting && (timeOutCount < countLimit)) 
	       myInterupt() 
	       drain(poport) 
	       myKeepWaiting = !daisyJobCheck(job)
	       (if myKeepWaiting then
		 unless(waited 
			fprintf(poport "Waiting for: %s. " job) 
			drain(poport) 
			) 
		 (if waited then 
		   timeOutCount++ ;; Problem here if job is still waiting in LSF queue 
		   ) 
		 waited = t 
		 sleep(10) 
		 sleep(10) 
		 sleep(10) 
		 ) ; if
	       ) ; while 
	     (if waited then 
	       (if  (timeOutCount < countLimit) then 
		 fprintf(poport (strcat "Job completed. " getCurrentTime() "\n")) 
		 else 
		 fprintf(poport (strcat "Job timed out! " getCurrentTime() "\n")) 
		 ) 
	       drain(poport)
	       ) 
	     ) ; foreach 
    ) ;let 
  ) ; procedure 


(defun stringPart (intext startIndex endOffset) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Christer Jansson 200x-xx-xx
  ;; Outputs the charachters between startIndex and string length - endOffset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (let ((outstring "") (i 0)) 
    (if (strlen(intext) - startIndex - endOffset > 0) then 
      (for index startIndex (strlen(intext)-endOffset) 
	   outstring = (strcat outstring getchar(intext index)) 
	   )
      else 
      (setq outstring "")
      )
    outstring
    ))


(defun schNodeNameToAllExtNodeNames (nodeToSaveList)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Christer Jansson 2007-06-20
  ;; 
  ;; This function convert: 
  ;; list("/a/b/c<3>/nod" "/v") 
  ;; into:
  ;; list("/a_b_c_3__nod" "/a/b_c_3__nod" "/a/b/c_3__nod" "/a/b/c<3>/nod" "/a/b/c<3>/nod"  "/v" "/v")
  ;; 
  ;; For use with Calibre extraction.
  ;;
  ;; Known bugs: Duplicate elements
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let (tmpStringList tmpString (tmpNodeList nil) nodeBaseName index_i index_j subStringList)
    (foreach nodeToSave nodeToSaveList
	     tmpStringList = parseString(nodeToSave "/")
	     
	     nodeBaseName = "/" ; (strcat "/" nth(0 tmpStringList))
	     (for index_i 0 length(tmpStringList)
		  subStringList = nthelem(index_i tmpStringList)
		  (when subStringList 
		    nodeBaseName = (strcat nodeBaseName nthelem(index_i tmpStringList) "/")
		    );when
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  ;; Extracted syntax after the index_i
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  tmpString = nodeBaseName
		  (for index_j (index_i+1) length(tmpStringList)
		       subStringList = parseString(nthelem(index_j tmpStringList) "<>")
		       (if length(subStringList)==2 then
			   tmpString = (strcat tmpString  nth(0 subStringList) "_" nth(1 subStringList) "__")
			   else
			   tmpString = (strcat tmpString nth(0 subStringList) "_")
			   )
		       ) ; for j
		  tmpString = stringPart(tmpString 1 1)
		  tmpNodeList = append(tmpNodeList list(tmpString))
		  ) ; for i
	     ) ; foreach
    tmpNodeList
    ) ; let
  ) ;defun



(defun asiSumLinBus (netName @optional (netStart nil) (netWidth nil))
  ;; Sum the contents of a bus.  
  (let ( (busVal 0) (busList nil) )
    (if (not netStart)        (setq netStart 0))
    (if (not netWidth)        (setq netWidth 1))
    (for wire_no netStart netWidth
	 (setq busList (append busList (list (VT (sprintf nil "/%s<%d>" netName wire_no))))))    
    (foreach wave busList
	     (setq busVal  
		   (plus busVal  wave)))
    busVal))

(defun asiAnnotateDesignVariables ()		  
  ;; Takes the current design variables and adds them to the schematic as a note.
  (let ( (desVarList (asiGetDesignVarList (asiGetCurrentSession)))
	 (noteText "Design variables: \n") ) ; Initial note text
    
    (foreach desVar desVarList	      
	     (setq noteText (strcat noteText "   " (car desVar) " = "  (cadr desVar) "\n") ))
    
    (setq noteText (strcat noteText "Copied " (getCurrentTime)))
    (schCreateNoteLabel (getCurrentWindow)->cellView (list 2.0 2.0) 
			noteText "lowerCenter" "R0" "swedish" 0.125 "normalLabel")))

(defun returnTransistorRegion (regionNo @optional (shortStr nil))
  (let (   
	(l_regionLookUp (list (list 0 (list "Cut-off" "cut")) 
			      (list 1 (list "Linear" "lin"))
			      (list 2 (list "Saturation" "sat"))
			      (list 3 (list "Subthreshold" "sub"))
			      (list 4 (list "Break-down" "brk")))))
    (if shortStr
	(cadadr (assoc regionNo l_regionLookUp))
      (caadr (assoc regionNo l_regionLookUp)))))

(defun presentRegionsPopUp ()		  
  ;; Used for short-cut key. Uses a file to temporarily store the 
  ;; data and then display it in a pop-up window.

  ;; Could be updated to return a list of components, their path, and their operation  regions, margins. 
  ;; Would be useful for plotting results.
  (let ( (selectedTransistors nil)
	 (operationRegion nil)
	 (operationMargin 0)
	 (selectedObjects (geGetSelectedSet))
	 (l_transistors  (list "nch" "nch_25" "pch_25" "pmos4" "nmos4" "nmos1v" "pmos1v" "nmos1v_lvt" "pmos1v_lvt"
			       "P_33_MM" "P_18_MM" "N_33_MM" "N_18_MM"
			       "nsvt25rf" "psvt25rf" "nsvtrf" "psvtrf"
			       "nsvt25" "psvt25" "nsvt" "psvt"
			       			       "nsvt33" "psvt33" 
			       		"phvtlprpo" "nhvtlprpo"

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
			   (setq mathExp  (strcat "(getData \"" hierPath "/"  singleObject~>name  "\" ?result \"dcOpInfo-info\")~>region" ))
			   (setq mathExp2 (strcat "(getData \"" hierPath "/"  singleObject~>name  "\" ?result \"dcOpInfo-info\")~>vds" ))
			   (setq mathExp3 (strcat "(getData \"" hierPath "/"  singleObject~>name  "\" ?result \"dcOpInfo-info\")~>vdsat" ))
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
					  (strcat "getData(\"" hierPath "/" singleObject->baseName 
						  (sprintf nil "<%d>" instNo)  "\" ?result \"dcOpInfo-info\")~>region" ))
				    (setq mathExp2 (strcat "getData(\"" hierPath "/"  singleObject~>baseName (sprintf nil "<%d>" instNo)  "\" ?result \"dcOpInfo-info\")~>vds" ))
				    (setq mathExp3 (strcat "getData(\"" hierPath "/"  singleObject~>baseName (sprintf nil "<%d>" instNo) "\" ?result \"dcOpInfo-info\")~>vdsat" ))
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


(defun asiAnnotateNetVoltage ()		  ;_Sep  9 04 wikner 0
  ;; Used for short-cut key. Uses a file to temporarily store the 
  ;; data and then display it in a pop-up window. 
  (let ( (selectedObjects (geGetSelectedSet))
	 (hierPath "") 
	 (mathExp "") 
	 (objNameStripped nil) )
    
    (if selectedObjects then
      (progn
	(setq oport (outfile (setq fileName (makeTempFileName "/tmp/netVoltages"))))
	(if oport
	    (progn
	      
	      (fprintf oport "\n")
	      (fprintf oport "======================================================= \n")
	      (fprintf oport "DC simulation results from %s\n" (getCurrentWindow)-> cellView -> cellName)
	      (fprintf oport "List of nets and their voltages            \n")
	      (fprintf oport "=======================================================   \n")
	      (fprintf oport " Cell Name       Net Name          Voltage [V]            \n")
	      (fprintf oport "-------------------------------------------------------   \n\n")
	      
	      (foreach singleObject selectedObjects 		       
		       (if (singleObject->net)	then
			 (setq hierPath (geGetInstHier))
			 (if (equal hierPath "/") (setq hierPath "") )	
			 
			 (foreach wireName singleObject->net->sigNames	 
				  (printf "%s\n" wireName)
				  (setq mathExp (strcat "getData(\"" hierPath "/"  wireName  "\" ?result \"dcOp-dc\")" ))
				  (setq netValue (evalstring mathExp))	
				  (printf "%s" mathExp)
				  (if netValue then 
				    (fprintf oport "%10s     %9s     :        %f \n" hierPath wireName netValue)
				    else	   
				    (fprintf oport "%10s     %9s     :        %s \n" hierPath wireName "Undefined!")) )))
	      (fprintf oport  "\n=======================================================\n")
	      (close oport)
	      (view fileName nil (strcat "Net voltages -- " (getCurrentTime) " -- " (getCurrentWindow)-> cellView -> cellName))
	      (deleteFile fileName))
	  (error "Couldn't open output file")))) ))


(defun daisyTraverseCornerFile ( fileName )
  (let (( variables "")
	( variablesList (list))
	( cornerList (list))
	( iport nil ) 
	(parametersList (list "start with something"))
	( cornerNo 0))
    
    (if (isFileName fileName)
	(progn
	  (setq iport (infile fileName))
	  (fscanf iport "%s"   variables)
	  ;;(printf       "%s\n" variables)
	  (setq variablesList (parseString variables ","))	  
	  (while (lessp (fileTell  iport) (fileLength fileName))
	    (fscanf iport "%s\n" parameters)	  
	    (setq parametersList (parseString parameters ","))
	    ;;(printf "---> %s\n" parameters)	  
	    ;;(printf "%L\n" parametersList)	  
	    (setq lineList (list))
	    (setq parseVariables variablesList)	  
	    (foreach param parametersList
		     (setq lineList (append lineList (list (list (car parseVariables) param))))		   
		     (setq parseVariables (cdr parseVariables)))	  
	    (setq cornerList (append cornerList (list (list cornerNo lineList))))	  
	    (setq cornerNo (plus cornerNo 1)))	
	  (close iport)))
    cornerList))

(defun returnCorner (cornerNo component cornerList)
  (cadr (assoc component (cadr (assoc cornerNo cornerList)))))

(defun returnCornerState  (cornerNo cornerList)
  (let ( (cornerStateList (list)))
    (foreach header (cadr (assoc cornerNo cornerList))
	     (setq cornerStateList (append cornerStateList (list (cadr header)))))
    cornerStateList))

(defun returnCornerHeader (cornerList)
  (let ( (cornerHeaderList (list)))
    (foreach header
	     (cadr (assoc (caar cornerList) cornerList))
	     (setq cornerHeaderList (append cornerHeaderList (list (car header)))))
    cornerHeaderList))

(defun returnTsmcCorner (cornerList cornerNo component)
;; This function only returns the correct string, since in 
;; the 90 and 80-nm kits there were slight discrepancies in 
;; nomenclature.
  (if (or (equal (getShellEnvVar "CDSPROCESSNAME") "tsmcN80")
	  (equal (getShellEnvVar "CDSPROCESSNAME") "tsmcN90"))
      (strcat 
       (returnCorner cornerNo component cornerList) 
       (if (equal "std" component) "" "_")
       (if (equal "std" component) "" component))
    )
  (if (or (equal (getShellEnvVar "CDSPROCESSNAME") "tsmcN40")
	  nil)
      ;; In the 40-nm kit there is only one option "top"
      ;; so it makes no sense to call this function.      
      (strcat 
       component "_"
       (returnCorner cornerNo component cornerList)  )
    )
   )

(defun daisyPrintCorner (cornerList cornerNo)
  (let ((cornerHeader (list))
	(cornerNoList (list)))
    ;; Find the header
    (printf "  ")
    (foreach subComponent (cadr (assoc (if (listp cornerNo) (car cornerNo) cornerNo) cornerList))
	     (setq cornerHeader (append cornerHeader (list (car subComponent))))
	     (printf "%9s " (car subComponent)))
    (printf "\n")
    (setq cornerNoList cornerNo)
    (if (listp cornerNoList) 
	(foreach cornerNo cornerNoList
		 (printf "%2d " cornerNo)
		 (foreach subComponent (cadr (assoc cornerNo cornerList))
			  (printf "%9s " (cadr subComponent)))
		 (printf "\n"))
      (progn 
	(printf "%2d " cornerNo)
	(foreach subComponent (cadr (assoc cornerNo cornerList))
		 (printf "%9s " (cadr subComponent)))
	(printf "\n"))))
  t)

(defmethod abMakeWaveform ( xvalues  
			    yvalues
			    @optional (dataType 'double))
  (let (xVec yVec wave)
    (when
        (and (listp xvalues) (equal (length xvalues) 
				    (length yvalues)))
      (setq wave (drCreateEmptyWaveform))
      (setq xVec (drCreateVec dataType xvalues))
      (setq yVec (drCreateVec dataType yvalues))
      (drPutWaveformXVec wave xVec)
      (drPutWaveformYVec wave yVec)
      wave)))


;; Macro to reduce code...

(defun updateCornersTsmc (cornerList cornerNo @optional (modelsPtr nil))
  (if (or (equal (getShellEnvVar "CDSPROCESSNAME") "tsmcN90")
	  (equal (getShellEnvVar "CDSPROCESSNAME") "tsmcN80"))
      (modelFile
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "std"        ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "bip"        ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "dio"        ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "dio_lvt"    ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "dio_33"     ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "dio_dnw"    ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "dio_na"     ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "dio_hvt"    ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "dio_na33"   ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "bip_npn"    ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "mos_cap"    ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "mos_cap_33" ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "res"        ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "33"         ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "na"         ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "hvt"        ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "na33"       ))
       (list  modelsPtr  (returnTsmcCorner cornerList  cornerNo "lvt"        )) 
       '("/home/jawi/83xx//daisy/pdkSpecific/tsmcN90/models/cln90g_lk.scs" "tt_dio_esd")
       )
    )
  (if (or (equal (getShellEnvVar "CDSPROCESSNAME") "tsmcN40")
	  nil)
      (modelFile
       (list  (if modelsPtr modelsPtr 
		
		(strcat 
		 (getShellEnvVar "DAISYAREA") 
		 "/pdkSpecific/" 
		 (getShellEnvVar "CDSPROCESSNAME") 
		 "/models/"
		 "toplevel.scs")
		)
		(returnTsmcCorner cornerList  cornerNo "top"        ))   
       )
    )
  )
  
(defun daisyStandardCornerList ()
  ;; Return the standard corner list for the given process node.
  ;; This list should probably also depend on the project in the
  ;; future.
  (daisyTraverseCornerFile 
   (strcat (getShellEnvVar "DAISYAREA") 
	   "/pdkSpecific/"
	   (getShellEnvVar "CDSPROCESSNAME")
	   "/simulation/corners/daisyCorners_compact.csv")))

(defun daisyOpenSimResFile (desLib desCell simTag)
  ;; Instead of creating a dummy name, one could increase the revision 
  ;; and/or tag with date, etc.
  ;; This function can be used to increase the revision too

  (let (
	(csvFileName "")
	)
    (setq csvFileName 
	  (strcat 
	   (getShellEnvVar "CDSSIMPATH") "/results/" 
	   desLib "_" desCell "_" simTag ))
    (if (isFile (strcat csvFileName ".csv"))
	(setq csvFileName (strcat (makeTempFileName csvFileName) ".csv"))
      (setq csvFileName (strcat csvFileName ".csv")))
    (outfile csvFileName) 
    ))

(defun daisyWriteCsvHeader (csvPort headerList)
  (if csvPort 
      (fprintf csvPort
	       (csvList (append (list "No")
				(append (returnCornerHeader cornerList)			  
					headerList)))))
  )

(defun daisyWriteCsvRow (csvPort cornerNo cornerList resultsList)
  (let (( interRow (list)))
    (foreach result resultsList 
	     (setq interRow 
		   (append interRow			   
			   (list 
			    (if (stringp result) 
				result
			      (sprintf nil "%5.5f" (daisyFloat result)))
			    )
			   )
		   ))
    (if csvPort
	(fprintf csvPort 
		 (csvList 
		  (append (list (sprintf nil "%d" cornerNo ) ) 
			  (append (returnCornerState cornerNo cornerList)
				  interRow)))))))

(defun setCornerTsmc (cornerNo 
		      @optional 
		      (cornerList nil) 
		      (session nil))  
  (let 
      (
       (loadedModel nil) 
       (newModelList nil) 
       (modelPath "") 
       (changeOfModels nil)
       (pointList nil) 
       (identifier nil)
       )

    (if (not cornerList) 
	(setq cornerList
	      (daisyTraverseCornerFile 
	       (strcat 
		(getShellEnvVar "DAISYAREA") 
		"/pdkSpecific/"
		(getShellEnvVar "CDSPROCESSNAME")
		"/sim/corners/daisyCorners_compact.csv"))))
    
    (if (not session) 
	(setq session (asiGetCurrentSession)))      
    (setq loadedModel 
	  (asiGetModelLibSelectionList session))    
    (setq changeOfModels 
	  (cadr (assoc cornerNo cornerList)))
    
    (printf "%L \n" loadedModel)

    ;; This should be a single-iteration loop in the tsmcN40 case...
    ;; all models have been packed into one call to the main model card.
    ;; we could add an if statement here to save some time if we like.
    (foreach model loadedModel
	     
	     (setq modelPath (car model))
	     (setq currentModel (cadr model))
	     (setq pointList (cdr (parseString currentModel "_")))	     
	     (setq identifier nil)	     
	     
	     ;; This is the tsmcN90 and tsmcN80 specifications
	     (if (not pointList)
		 (setq identifier "std")	       	       
	       (progn
		 (setq identifier (car pointList))
		 ;; This is also for the 80 and 90-nm kits
		 (foreach point (cdr pointList)			  
			  (setq identifier (strcat identifier "_" point)))))
	     ;;(printf "%L " (cadr (assoc identifier changeOfModels)))	     
	     (setq newCorner (strcat (if (not (cadr (assoc identifier changeOfModels))) 
					 "tt" 
				       (cadr (assoc identifier changeOfModels)))
				     (if (equal identifier "std") "" (strcat "_"  identifier))))
	     (setq newModelList
		   (append newModelList (list (list modelPath newCorner )))))
    
    ;; added by nian 
    (if (equal (getShellEnvVar "USER") "nian")  
	(setq newModelList
	      (append newModelList (list (list modelPath  "ndio_esd" "tt")))) )
    
    
    (if (equal (getShellEnvVar "CDSPROCESSNAME") "tsmcN40")
	(progn
	  ;; (printf "hehehe")
	  (setq newModelList (list (list modelPath (strcat "top_" (cadr (assoc "top" changeOfModels))))))))
    
    (printf "%L \n" newModelList)
      
    
    (asiGetEnvOption (asiGetCurrentSession) "modelFiles")->value = newModelList)
  ;; print whole model list
    ;; (fprintf poport "%L\n"	newModelList)
    
    )

(defun setCornerDesVar (cornerNo 
			@optional 
			(cornerList nil) 
			(session nil))  
  (let ((loadedModel    nil) 
	(newModelList   nil) 
	(modelPath      "") 
	(changeOfModels nil)
	(pointList      nil) 
	(identifier     nil))    

    (if (not cornerList) 
	(setq cornerList
	      (daisyTraverseCornerFile 
	       (strcat 
		(getShellEnvVar "DAISYAREA") 
		"/pdkSpecific/"
		(getShellEnvVar "CDSPROCESSNAME")
		"/simulation/corners/daisyCorners_compact.csv"))))
    (if (not session) 
	(setq session (asiGetCurrentSession)))  
 
    (foreach masterPoint (list "vpwr10"  
			       (if (equal 
				    (getShellEnvVar "CDSPROCESSNAME")
				    "tsmcN40")
				   "vpwr25" "vpwr33") 
			       "temp")
	     ;; (printf "Setting design variable ... \n")
	     
	     (asiAddDesignVarList (asiGetCurrentSession) 
				  (list (list (cadr (assoc masterPoint 
							   (list 
							    (list "vpwr10" "vccr10") 
							    (list "vpwr11" "vccr11") 
							    (list "vpwr25" "vccr25") 
							    (list "vpwr33" "vccr33") 
							    (list "temp" "temp"))))
					      (cadr (assoc masterPoint 
							   (cadr (assoc cornerNo cornerList)))))))
	     (printf "%s\n" (cadr (assoc masterPoint (cadr (assoc cornerNo cornerList)))))
	     (if (equal masterPoint "temp")     
		 (progn 
		   (asiSetSimOptionVal session "temp" (cadr (assoc masterPoint (cadr (assoc cornerNo cornerList)))))
		   session->data->env->data->opts->data->temp->value = (cadr (assoc masterPoint (cadr (assoc cornerNo cornerList))))
		   (envSetVal "spectre.opts" "temp" 'string (cadr (assoc masterPoint (cadr (assoc cornerNo cornerList))))))))))
;;(asiAddDesignVarList (asiGetCurrentSession) (list (list "vccr10" (sprintf nil "%d" random(1)))))
(defun setCorner (cornerNo 
		  @optional 
		  (cornerList nil) 
		  (session nil))
  ;; Function to be called from e.g. the ciw for simpler access
  ;; to the model setup and to adapt to daisy-defined corners.

;;  (printf "Setting design variables ... \n")
  
  ;; Update the non-tsmc corners first, i.e., the daisy corners
  (setCornerDesVar cornerNo cornerList session)
  

;;  (printf "Setting corner ...\n")

  ;; Update the tsmc corners.
  (setCornerTsmc cornerNo cornerList session)
  )

(defun daisySatMargin (pathName transName)

  (list (returnTransistorRegion (OP (strcat pathName transName) "region") t) 
	(sprintf nil "%5.5f" (difference (OP (strcat pathName transName) "vds")  
					 (OP (strcat pathName transName) "vdsat")))))
(defun  daisySatMargins (transNamesList)
  ;; transNamesList consists of a list as:
  ;; (list     ( list   ( list cellPath1  cellAlias1 ) (list trans1 trans2 trans3 ...) )
  ;;           ( list   ( list cellPath2  cellAlias2 ) (list trans1 trans2 trans3 ...) ) 
  ;;         )
  ;; cellAlias is used for pretty print
  ;; 
  (let ((resultsList nil) (cellPath ""))
    (foreach transBundle transNamesList
	     (setq cellPath (caar transBundle))
	     (foreach transName (cadr transBundle)
		      (setq resultsList (append resultsList (daisySatMargin cellPath transName)))))
    resultsList))

(defun daisyTransInList (transNamesList)
  ;; transNamesList consists of a list as:
  ;; (list     ( list   ( list cellPath1  cellAlias1 ) (list trans1 trans2 trans3 ...) )
  ;;           ( list   ( list cellPath2  cellAlias2 ) (list trans1 trans2 trans3 ...) ) 
  ;;         )
  ;; cellAlias is used for pretty print
  ;; 
  (let ((headerList nil))
    (foreach transBundle transNamesList
	     (if (lessp (length transBundle) 2)
		 (setq alias nil)
	       (setq alias (cadar transBundle)))
	     (foreach transName (cadr transBundle)
		      (foreach tag (list "reg" "marg")
			       (setq headerList (append headerList (list (strcat (if alias (strcat alias  ".") "") transName  "." tag)))))))
    headerList))
   
;;(daisySatMargins (list (list (list "/bgcore/IcoreAmp/")
;;			      (list  "M1" "M2" "M3"))
;;			(list (list "/bgcore/Istartup/" )
;;			      (list "M0" "M1" "M2"))))

(defun daisyFloat (number)
  (if (boundp 'number)
      (if number
	  (float number)
	0.00000)
    0.0000))

(defun daisyPresentRegionsPopUp ()		  

  ;; Used for short-cut key. Uses a file to temporarily store the 
  ;; data and then display it in a pop-up window.

  ;; Could be updated to return a list of components, their path, and their operation  regions, margins. 
  ;; Would be useful for plotting results.

  (let ( (selectedTransistors nil)
	 (operationRegion nil)
	 (operationMargin 0)
	 (selectedObjects (geGetSelectedSet))
	 (l_transistors  
	  (list "nch" "nch_25" "pch_25" "pmos4" "nmos4" "nmos1v" "pmos1v" "nmos1v_lvt" "pmos1v_lvt"
		"P_33_MM" "P_18_MM" "N_33_MM" "N_18_MM"
		"nsvt25rf" "psvt25rf" "nsvtrf" "psvtrf"
		"nsvt25" "psvt25" "nsvt" "psvt"
		"nsvt33" "psvt33"
		"nsvtlp" "psvtlp"
		"phvtlprpo" "nhvtlprpo"
		"nsvtgp" "psvtgp"
		"nlvtgp" "plvtgp"
		"nlvtlp" "plvtlp" "nhvtlp"
		"nhvtgp" "phvtgp"
		"nhvt25" "phvt25" 
		"nhvt" "phvt"
		"nlvt25" "plvt25" "nlvt" "plvt"
		"nch_33" "nch_33_dnw" "nch_33_dnwx" 
		"nch_33x" "nch_dnw" "nch_dnwx" "nch_hvt" 
		"nch_hvt_dnw" "nch_hvt_dnwx" "nch_hvtx" 
		"nch_lvt" "nch_lvt_dnw" "nch_lvt_dnwx" 
		"nch_lvtx" "nch_na" "nch_na33" "nch_na33x" 
		"nch_nax" "nchx" "pch" "pch_33" 
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
	      (fprintf oport " (Hierarchical) Transistor      Operation region        Margin  \n")
	      (fprintf oport "---------------------------------------------------------------  \n\n")
	      
	      (foreach singleObject selectedObjects 
		       (when (member singleObject~>cellName l_transistors)			 
			 (setq hierPath (geGetInstHier))
			 (if (equal hierPath "/") (setq hierPath "") )			 
			 (if (equal singleObject->numInst 1) then
			   (setq transName (strcat hierPath  (if (equal "" hierPath) "" "/") singleObject->name))
			   ;; (printf "%s\n" transName)
			   (rexCompile "/")
			   (setq transName (rexReplace transName "" 1))
			   ;; (printf "%s\n" transName)
			   (rexCompile "/")
			   (setq transName (rexReplace transName "." 0))
			   ;; (printf "%s\n" transName)

			   ;; STM fix
			   ;; The if statement should change ... 
			   (if (member singleObject->cellName (list "nhvtlp" "nsvtlp" "psvtlp" "nlvtlp" "plvtlp"  "nlvtgp" "plvtgp"  "nsvtgp" "psvtgp"  "nhvtgp" "phvtgp"))
			       (setq transName (strcat transName "." 
						       (if (member (getShellEnvVar "CDSPROCESSNAME") (list "stm040lp")) "x" "")
						       (if (member (getShellEnvVar "CDSPROCESSNAME") (list "stm090rf")) "M" "m")
						       "1")))

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
			   ;; (printf "Iterated instance. Assuming <0:%d> or <%d:0>.\n" singleObject->numInst-1 singleObject->numInst-1)
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
				    
				    (setq transName (strcat hierPath  (if (equal "" hierPath) "" "/") singleObject->baseName))
;;				    (printf "%s\n" transName)
				    (rexCompile "/")
				    (setq transName (rexReplace transName "" 1))
	;;			    (printf "%s\n" transName)
				    (rexCompile "/")
				    (setq transName (rexReplace transName "." 0))
		;;		    (printf "%s\n" transName)
				    (setq transName (sprintf nil "%s_%d" transName instNo))
				    
				    ;; STM fix
				    (if (member singleObject->cellName 
						(list "nhvtlp" "nsvtlp" "psvtlp" "nlvtlp" "plvtlp"  "nlvtgp" "plvtgp"  "nsvtgp" "psvtgp"  "nhvtgp" "phvtgp"))
					(setq transName (strcat transName "." 
								(if (member (getShellEnvVar "CDSPROCESSNAME") (list "stm040lp")) "x" "")
								(if (member (getShellEnvVar "CDSPROCESSNAME") (list "stm090rf")) "M" "m")
								"1")))

				    (setq mathExp  (strcat "(pv \"" transName  "\" \"region\" ?result \"dcOpInfo-info\")" ))
				    (setq mathExp2 (strcat "(pv \"" transName  "\" \"vds\"    ?result \"dcOpInfo-info\")" ))
				    (setq mathExp3 (strcat "(pv \"" transName  "\" \"vds\"    ?result \"dcOpInfo-info\")" ))
				    (setq operationRegion (evalstring mathExp))	
;;				    (printf "%s" mathExp)

				    
				    (setq operationMargin (if (equal "p" (substring singleObject~>cellName 1 1)) 
							      -(evalstring mathExp2)+(evalstring mathExp3)
							      (evalstring mathExp2)-(evalstring mathExp3)))
				    
				    (setq operationRegion (evalstring mathExp))	
				    (if operationRegion then 
				      (setq operationRegion (nth (evalstring mathExp) l_regionLookUp))
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

