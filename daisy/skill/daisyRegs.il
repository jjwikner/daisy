;;;start SKILL program for adding mozilla view

(printf " ... loading %s.il ...\n" "daisyRegs")

(defun  CCSmozillaDataTrig ( argList )
  (let ( cellName file libId objId tmplFile 
		  viewFileName viewName viewType )
    
    cellName = argList->cellName
    libId    = argList->libId
    viewName = argList->viewName
    viewType = argList->viewType
    
    viewFileName = (ddMapGetViewTypeFileName viewType )
    
    (if  objId = (ddGetObj libId~>name cellName viewName viewFileName nil "r" )
	 then
	 file = (ddGetObjWritePath objId )
	 else
	 objId = (ddGetObj libId~>name cellName viewName viewFileName nil "w" )
	 file = (ddGetObjWritePath objId )
	 tmplFile = (strcat (getShellEnvVar "DAISYAREA") "/template.html")
	 (unless (isFile tmplFile )
	   (error "Cannot find template file <%s>\n" tmplFile ))
	 (system (sprintf nil "cp %s %s" tmplFile file ))
	 (ipcSleep 2 )
	 (printf "Created the mozilla cellView!\n" ))
    (system (sprintf nil "/usr/bin/firefox `cat %s` &" file ))
    t ) )

(defun  CCSmozillaAppTrig (argL) t )
(defun  CCSmozillaEnableTrig (@optional argL) t )

(defun  CCSmozillaRegister ()
  (deRegApp 
   ?appName "mozilla"
   ?appTrigger 'CCSmozillaAppTrig ; Dummy function
   ?dataTrigger 'CCSmozillaDataTrig
   ?enableTrigger 'CCSmozillaEnableTrig ; Dummy function
   ?viewType "url"
   ?widgetType "none"))

(CCSmozillaRegister)
;;(envSetVal "graphic" "viewNameToTypeList" 'string "url")

(defun  openPdfDataTrigger (argList)
  (let  (mode(getq(argList accessMode)) 
	     libName(getq(argList libName)) cellName(getq(argList cellName)) 
	     viewName(getq(argList viewName)) fileId fileName)
    
    fileId=(ddGetObj libName cellName viewName "document.pdf")
    (if fileId
	(progn
	  fileName=(strcat (ddGetObjWritePath (ddGetObj libName cellName viewName)) "/document.pdf")
	  (ipcBeginProcess (sprintf nil "acroread %s" fileName)))
      (progn
	fileName=(strcat (ddGetObjWritePath (ddGetObj libName cellName viewName)) "/document.pdf")
	(system (sprintf nil "echo \"document.pdf\" > %s/master.tag"
			 (ddGetObjWritePath (ddGetObj libName cellName viewName))))
	(system (sprintf nil "cp %s %s" 
			 (strcat (getShellEnvVar "DAISYAREA") "/doc/pdfs/Lecture6.98.pdf")
			 (ddGetObjWritePath (ddGetObj libName cellName viewName))))
	(system (sprintf nil "ln -s %s/%s %s/document.pdf" 
			 (ddGetObjWritePath (ddGetObj libName cellName viewName))
			 "Lecture6.98.pdf"
			 (ddGetObjWritePath (ddGetObj libName cellName viewName))))
	(ipcBeginProcess (sprintf nil "acroread %s" fileName))))
  t))

(defun  CCSpdfAppTrig (argL) t )
(defun  CCSpdfEnableTrig (@optional argL) t )

(defun  CCSpdfRegister ()
  (deRegApp 
   ?appName "acroread"
   ?appTrigger 'CCSpdfAppTrig ; Dummy function
   ?dataTrigger 'openPdfDataTrigger
   ?enableTrigger 'CCSpdfEnableTrig ; Dummy function
   ?viewType "pdf"
   ?widgetType "none"))
(CCSpdfRegister)
;;(envSetVal "graphic" "viewNameToTypeList" 'string "pdf")

(defun  openOooDataTrigger (argList)
  (let  (mode(getq(argList accessMode)) 
	     libName(getq(argList libName)) cellName(getq(argList cellName)) 
	     viewName(getq(argList viewName)) fileId fileName)
    (setq fileId (ddGetObj libName cellName viewName "document"))
    (printf "---------------> %s %s %s  \n" libName cellName viewName)
    (if fileId
	(progn
	  fileName = (strcat (ddGetObjWritePath (ddGetObj libName cellName viewName)) "/document")
	  (printf "Launching Openoffice")
	  (ipcBeginProcess (sprintf nil "/usr/bin/openoffice.org  %s %s" 
				    (case viewName 
					  ((equal "csv")             "-calc")
					  ((equal "progress")        "-calc")
					  ((equal "spreadsheet")     "-calc")
					  ((equal "doc")             "-write")
					  ((equal "diagram")         "-draw") 
					  ((equal "present")         "-impress")
					  ((equal "ppt")             "-impress")
					  ((else "-write")))
				    fileName)))
      (progn
	;; Creating a new file.
	(printf "---------------> %s %s %s  \n" libName cellName viewName)
	fileName=(strcat (ddGetObjWritePath (ddGetObj libName cellName viewName)) "/document")
	(printf "---------------> %s %s %s  \n" libName cellName viewName)
	(system (sprintf nil "echo \"document\" > %s/master.tag"
			 (ddGetObjWritePath (ddGetObj libName cellName viewName))))
	(system (sprintf nil "cp %s.%s %s/template_%s_%s_%s.%s" 
			 (strcat (getShellEnvVar "DAISYAREA") "/doc/templates/template")
			 (case viewName 
			       ((equal "csv")             "csv")
			       ((equal "progress")        "xls")
			       ((equal "spreadsheet")     "xls")
			       ((equal "doc")             "doc")
			       ((equal "diagram")         "odg") 
			       ((equal "present")         "ppt")
			       ((equal "ppt")             "ppt")
			       ((else "doc")))
			 (ddGetObjWritePath (ddGetObj libName cellName viewName))
			 libName cellName viewName
			 (case viewName 
			       ((equal "csv")             "csv")
			       ((equal "progress")        "xls")
			       ((equal "spreadsheet")     "xls")
			       ((equal "doc")             "doc")
			       ((equal "diagram")         "odg") 
			       ((equal "present")         "ppt")
			       ((equal "ppt")             "ppt")
			       ((else "doc")))
			 ))
	(system (sprintf nil "ln -s %s/template_%s_%s_%s.%s %s/document" 
			 (ddGetObjWritePath (ddGetObj libName cellName viewName))
			 libName cellName viewName
			 (case viewName 
			       ((equal "csv")             "csv")
			       ((equal "progress")        "xls")
			       ((equal "spreadsheet")     "xls")
			       ((equal "doc")             "doc")
			       ((equal "diagram")         "odg") 
			       ((equal "present")         "ppt")
			       ((equal "ppt")             "ppt")
			       ((else "doc")))
			 (ddGetObjWritePath (ddGetObj libName cellName viewName))))	
	(printf "Launching Openoffice")
	(ipcBeginProcess (sprintf nil "/usr/bin/openoffice.org %s" fileName))))
    t))

(defun  CCSoooAppTrig (argL) t )
(defun  CCSoooEnableTrig (@optional argL) t )

(defun  CCSoooRegister ()
  (deRegApp 
   ?appName "openoffice"
   ?appTrigger 'CCSoooAppTrig ; Dummy function
   ?dataTrigger 'openOooDataTrigger
   ?enableTrigger 'CCSoooEnableTrig ; Dummy function
   ?viewType "doc"
   ?widgetType "none"))
(CCSoooRegister)


;;(envSetVal "graphic" "viewNameToTypeList" 'string "doc")

;; Skill view added ny nian 070402


(defun  CCSilViewerDataTrig ( argList )
  (let ( cellName 
	 file libId 
	 objId tmplFile 
	 viewFileName viewName 
	 viewType )
    
    cellName = argList->cellName
    libId    = argList->libId
    viewName = argList->viewName
    viewType = argList->viewType
    
    viewFileName = (ddMapGetViewTypeFileName viewType )
    
    (if  objId = (ddGetObj libId~>name cellName viewName viewFileName nil "r" )
	 then
	 file = (ddGetObjWritePath objId )
	 else
	 objId = (ddGetObj libId~>name cellName viewName viewFileName nil "w" )
	 file = (ddGetObjWritePath objId )
	 tmplFile = (strcat (getShellEnvVar "DAISYAREA") "/skill/genTemplate.il")
	 (unless (isFile tmplFile )
	   (error "Cannot find template file <%s>\n" tmplFile ))
	 (system (sprintf nil "cp %s %s" tmplFile file ))
	 (ipcSleep 2 )
	 (printf "Created the skill cellView!\n" ))
    (system (sprintf nil "/usr/bin/emacs %s &" file ))
    t ) )

(defun  CCSilViewerAppTrig (argL) t )
(defun  CCSilViewerEnableTrig (@optional argL) t )

(defun  CCSilViewerRegister ()
  (deRegApp 
   ?appName "emacs"
   ?appTrigger 'CCSilViewerAppTrig ; Dummy function
   ?dataTrigger 'CCSilViewerDataTrig
   ?enableTrigger 'CCSilViewerEnableTrig ; Dummy function
   ?viewType "il"
   ?widgetType "none"))

(CCSilViewerRegister)
;;(envSetVal "graphic" "viewNameToTypeList" 'string "url")

;; Gimp/PNG  view added ny jacobw

(defun  CCSpngViewerDataTrig ( argList )
  (let ( cellName file libId objId tmplFile 
		  viewFileName viewName viewType )
    
    cellName = argList->cellName
    libId    = argList->libId
    viewName = argList->viewName
    viewType = argList->viewType
    
    viewFileName = (ddMapGetViewTypeFileName viewType )
    
    (if  objId = (ddGetObj libId~>name cellName viewName viewFileName nil "r" )
	 then
	 file = (ddGetObjWritePath objId )
	 else
	 objId = (ddGetObj libId~>name cellName viewName viewFileName nil "w" )
	 file = (ddGetObjWritePath objId )
	 tmplFile = (strcat (getShellEnvVar "DAISYAREA") "/skill/graph.png")
	 (unless (isFile tmplFile )
	   (error "Cannot find template file <%s>\n" tmplFile ))
	 (system (sprintf nil "cp %s %s" tmplFile file ))
	 (ipcSleep 2 )
	 (printf "Created the png cellView!\n" ))
    (system (sprintf nil "gimp %s &" file ))
    t ) )

(defun  CCSpngViewerAppTrig (argL) t )
(defun  CCSpngViewerEnableTrig (@optional argL) t )

(defun  CCSpngViewerRegister ()
  (deRegApp 
   ?appName "gimp"
   ?appTrigger 'CCSpngViewerAppTrig ; Dummy function
   ?dataTrigger 'CCSpngViewerDataTrig
   ?enableTrigger 'CCSpngViewerEnableTrig ; Dummy function
   ?viewType "png"
   ?widgetType "none"))

(CCSpngViewerRegister)
;;(envSetVal "graphic" "viewNameToTypeList" 'string "url")
