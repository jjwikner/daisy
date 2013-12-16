;; Load Mentor Calibre Interactive

(printf " ... loading %s.il ...\n" "daisyMGC")


(if (and (getShellEnvVar "MGC_HOME") t)
    (load (strcat (getShellEnvVar "MGC_HOME") 		  
		  "/shared/pkgs/icv/tools/queryskl/calibre.skl")))

(setq  mgc_calibre_export_layout_template_file
       (strcat (getShellEnvVar "DAISYAREA")
	       "/verification/"
	       "mgc_calibre_export_layout_template_file"))

(setq  mgc_calibre_export_netlist_template_file
       (strcat (getShellEnvVar "DAISYAREA")
	       "/verification/"
	       "mgc_calibre_export_netlist_template_file"))

(setq streamOutKeys 
      (list	nil 
  		'runDir	(strcat (getShellEnvVar "USERAREA") "/log/")))
(setq streamInKeys 
      (list	nil 
  		'runDir	(strcat (getShellEnvVar "USERAREA") "/log/")))

(if (boundp 'cdlOutKeys)
    cdlOutKeys->incFILE = (strcat (getShellEnvVar "DAISYAREA") 
				  "/pdkSpecific/"
				  (getShellEnvVar "CDSPROCESSNAME")
				  "/pv/source_added")
    )

;; Calibre layout keys

(daisySetBindKey  "Layout" "AltShift<Key>r"   "(setq LISTOFDRCERRORS (displayDrcErrors))"  "Display a list of all calibre drc errors")
(daisySetBindKey  "Layout" "AltShift<Key>e"   "(foreach object LISTOFDRCERRORS (dbDeleteObject object)) " "Remove all the errors in the calibre drc error list")    

(daisySetBindKey  "Layout" "<Key>KP_Divide"   "(mgc_rve_hl_prev_error)" "Zoom to previous calibre error result")
(daisySetBindKey  "Layout" "<Key>KP_Multiply" "(mgc_rve_hl_curr_error)" "Zoom to current calibre error result")
(daisySetBindKey  "Layout" "<Key>KP_Subtract" "(mgc_rve_hl_next_error)" "Zoom to next calibre error result")
(daisySetBindKey  "Layout" "<Key>KP_Add"      "(mgc_rve_delete_markers)" "Delete all calibre error markers")
(daisySetBindKey  "Layout" "<Key>KP_Enter"    "(mgc_rve_hl_all_errors)" "Highlight all calibre error results")


(when (boundp 'mgc_eview_globals)
    mgc_eview_globals -> cellMapFile =  (strcat (getShellEnvVar "DAISYAREA") 
						"/pdkSpecific/"
						(getShellEnvVar "CDSPROCESSNAME")
						"/pv/calview.cellmap")
    mgc_eview_globals -> parasiticViewScale = 1.0
    mgc_eview_globals -> openCalview  = "edit"
    mgc_eview_globals -> extViewName  = "extractedC"
    mgc_eview_globals -> extViewType  = "schematic" ;  "maskLayout"	
    mgc_eview_globals -> resetPropertyList = (list "m=1" "simM=1" "fingers=1" "multi=1" "segments=1" "mf=1" "total_m=1") 
    mgc_eview_globals -> resetPropertyNames = "m=1 simM=1 fingers=1 multi=1 segments=1 mf=1 total_m=1"
    )


;; Some custom settings:

(when (boundp 'mgc_rve_strings)
  ;; Remove blink layers to avoid epileptical problems.
  mgc_rve_strings->hl_layer_set = mgc_rve_strings->hl_layer_set_h
  mgc_rve_strings->probeFileRoot = (strcat (getShellEnvVar "USERAREA") 
					   "/log/rveProbe/")
  )

(defun daisyCyCheck (runType @optional (hier  nil))
  
  (let ((libName "") (cellName ""))
    (setq libName  (hiGetCurrentWindow)->cellView->libName)
    (setq cellName (hiGetCurrentWindow)->cellView->cellName)
    
    (sh  (sprintf nil "cd %s; /home/proj/daisy/bin/cyCheck %s" 
		 (strcat (getShellEnvVar "VERAREA") 
			 libName 
			 "/" cellName 
			 "/" runType "/")
		 (strcat 			 
		  (if hier "h" " ")
		  runType " " 
		  cellName)))))

