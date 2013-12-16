;;--- adobeAcrobatReader

;;dummy app trigger to avoid the following error:
;;*Error* Root class viewType (PDF) must define data/app triggers!
(defun PDFAppTrig (argL) t )

;;dummy enable trigger to avoid the following:
;;abobeAcrobatReader is not enabled for "a" access.  
;; Changing access to "read".

(defun PDFEnableTrig (@optional argL) t)

;;register the PDF data trigger if not previously registered

(unless (deViewTypeIsRegistered "PDF")
  
  ;;register the PDF data trigger
  (deRegApp 
   ?viewType "PDF"
   ?appName "adobeAcrobatReader"
   ?dataTrigger 'fileDataTrig
   ?appTrigger 'PDFAppTrig
   ?enableTrigger 'PDFEnableTrig
   ?widgetType "none" ) )

;;--- adobeFrameMaker

;;dummy app trigger to avoid the following error:
;;*Error* Root class viewType (FM) must define data/app triggers!
(defun FMAppTrig (argL) t )

;;dummy enable trigger to avoid the following:
;;adobeFrameMaker is not enabled for "a" access.  
;; Changing access to "read".
(defun FMEnableTrig (@optional argL) t )

;;register the FM data trigger if not previously registered
(unless (deViewTypeIsRegistered "FM")
  
  ;;register the FM data trigger
  (deRegApp 
   ?viewType "FM"
   ?appName "adobeFrameMaker"
   ?dataTrigger 'fileDataTrig
   ?appTrigger 'FMAppTrig
   ?enableTrigger 'FMEnableTrig
   ?widgetType "none" ))

;;--- microsoftExcel

;;dummy app trigger to avoid the following error:
;;*Error* Root class viewType (XLS) must define data/app triggers!
(defun XLSAppTrig (argL) t )

;;dummy enable trigger to avoid the following:
;;msExcel is not enabled for "a" access.  Changing access to "read".
(defun XLSEnableTrig (@optional argL) t )

;;register the XLS data trigger if not previously registered
(unless (deViewTypeIsRegistered "XLS")

   ;;register the XLS data trigger
   (deRegApp 
      ?viewType "XLS"
      ?appName "microsoftExcel"
      ?dataTrigger 'fileDataTrig
      ?appTrigger 'XLSAppTrig
      ?enableTrigger 'XLSEnableTrig
      ?widgetType "none"
   )

) ;; unless

;;---  microsoftPowerPoint

;;dummy app trigger to avoid the following error:
;;*Error* Root class viewType (PPT) must define data/app triggers!
(defun PPTAppTrig (argL) t )

;;dummy enable trigger to avoid the following:
;;simpress is not enabled for "a" access.  Changing access to "read".
(defun PPTEnableTrig (@optional argL) t )

;;register the PPT data trigger if not previously registered
(unless (deViewTypeIsRegistered "PPT")

   ;;register the PPT data trigger
   (deRegApp 
      ?viewType "PPT"
      ?appName "microsoftPowerPoint"
      ?dataTrigger 'fileDataTrig
      ?appTrigger 'PPTAppTrig
      ?enableTrigger 'PPTEnableTrig
      ?widgetType "none"
   )

) ;; unless

;;--- microsoftWord

;;dummy app trigger to avoid the following error:
;;*Error* Root class viewType (DOC) must define data/app triggers!
(defun DOCAppTrig (argL) t )

;;dummy enable trigger to avoid the following:
;;msWord is not enabled for "a" access.  Changing access to "read".
(defun DOCEnableTrig (@optional argL) t )

;;register the DOC data trigger if not previously registered
(unless (deViewTypeIsRegistered "DOC")

   ;;register the DOC data trigger
   (deRegApp 
      ?viewType "DOC"
      ?appName "microsoftWord"
      ?dataTrigger 'fileDataTrig
      ?appTrigger 'DOCAppTrig
      ?enableTrigger 'DOCEnableTrig
      ?widgetType "none"
   )

) ;; unless

;;--- netscapeComposer

;;dummy app trigger to avoid the following error:
;;*Error* Root class viewType (HTML) must define data/app triggers!
(defun HTMLAppTrig (argL) t )

;;dummy enable trigger to avoid the following:
;;netscapeComposer is not enabled for "a" access.  Changing access to
;; "read".
(defun HTMLEnableTrig (@optional argL) t )

;;register the HTML data trigger if not previously registered
(unless (deViewTypeIsRegistered "HTML")

   ;;register the HTML data trigger
   (deRegApp 
      ?viewType "HTML"
      ?appName "netscapeComposer"
      ?dataTrigger 'fileDataTrig
      ?appTrigger 'HTMLAppTrig
      ?enableTrigger 'HTMLEnableTrig
      ?widgetType "none"
   )

) ;; unless

;;--- openofficeScalc

;;dummy app trigger to avoid the following error:
;;*Error* Root class viewType (SXC) must define data/app triggers!
(defun SXCAppTrig (argL) t )

;;dummy enable trigger
(defun SXCEnableTrig (@optional argL) t )

;;register the SXC data trigger if not previously registered
(unless (deViewTypeIsRegistered "SXC")

   ;;register the SXC data trigger
   (deRegApp 
      ?viewType "SXC"
      ?appName "openofficeScalc"
      ?dataTrigger 'fileDataTrig
      ?appTrigger 'SXCAppTrig
      ?enableTrigger 'SXCEnableTrig
      ?widgetType "none"
   )

) ;; unless

;;--- openofficeSimpress

;;dummy app trigger to avoid the following error:
;;*Error* Root class viewType (SXI) must define data/app triggers!
(defun SXIAppTrig (argL) t )

;;dummy enable trigger
(defun SXIEnableTrig (@optional argL) t )

;;register the SXI data trigger if not previously registered
(unless (deViewTypeIsRegistered "SXI")

   ;;register the SXI data trigger
   (deRegApp 
      ?viewType "SXI"
      ?appName "openofficeSimpress"
      ?dataTrigger 'fileDataTrig
      ?appTrigger 'SXIAppTrig
      ?enableTrigger 'SXIEnableTrig
      ?widgetType "none"
   )

) ;; unless

;;--- openofficeSwriter

;;dummy app trigger to avoid the following error:
;;*Error* Root class viewType (SXW) must define data/app triggers!
(defun SXWAppTrig (argL) t )

;;dummy enable trigger
(defun SXWEnableTrig (@optional argL) t )

;;register the SXW data trigger if not previously registered
(unless (deViewTypeIsRegistered "SXW")
  
  ;;register the SXW data trigger
  (deRegApp 
   ?viewType "SXW"
   ?appName "openofficeSwriter"
   ?dataTrigger 'fileDataTrig
   ?appTrigger 'SXWAppTrig
   ?enableTrigger 'SXWEnableTrig
   ?widgetType "none" ))

;;create object
;;This function creates the cell view object and
;;copies the file into it.
;;It returns the object id.
(defun create_object (libName cellName viewName file_path file)
  
  (prog (objId cmd dfII_file_path)
	;;get object Id
	objId = (ddGetObj libName cellName viewName)
	
	(unless objId~>isWritable  
	  ;;checkout call
	  ddAutoCheckout(objId))
	
	;;checkout failed, error out
	(unless objId~>isWritable
	  (error "View could not be opened for edit.") )

	;;delete object
	cmd=(sprintf nil "\\rm -rf %s" objId~>writePath)
	(unless (system cmd)
	  (error " ERROR - command failed: %s \n" cmd) )

	;;create object
	objId = (ddGetObj libName cellName viewName file nil "w" )
	
	;;get file path
	dfII_file_path = ddGetObjWritePath(objId)

	;;copy the file to the cell view location
	cmd=(sprintf nil "\\cp %s %s" file_path dfII_file_path)
	(unless (system cmd)
	  (error " ERROR - command failed: %s \n" cmd) )

	(system (sprintf nil "chmod +w %s" dfII_file_path))
	
	;;return the new object id
	(return  objId) ))

;;use existing
;;This fuction allows the user to select an existing file.
;;The new cell view object is created and the selected
;;file is copied.
;;It returns the object id.

(defun use_existing (libName cellName viewName)
  (prog (filter file program_file_extension file_extension file_path
		retdia warnText cmd objId dfII_file_path)
	;;get the filter from the data.reg file 
	;; (returns the Pattern (example "*.pdf"))
	
	filter = ddMapGetViewTypeFileName(viewType)
	
	;;set the program file extension
	program_file_extension=(car last(parseString(filter ".")))
	
	;;initialize
	file_path=""
	retdia=t
	
	;;display path input form until a correct file path is 
	;; entered or the user gives up
	while(!isFile(file_path) || (file_extension !=
						    program_file_extension)
		     
		     ;;display file selection form
		     file_path=createFileSelectionForm(filter)
		     
		     ;;selected file doesn't exist
		     (if !isFile(file_path) then
			 warnText = (sprintf nil "%L doesn't exist:\nPress YES to try
again" file_path)
			 ;;display warning box
			 retdia = (hiDisplayAppDBox 
				   ?name 'xx
				   ?dboxBanner " "
				   ?dboxText warnText
				   ?buttonLayout 'YesNo )
			 
			 ;;file exists, but extension doesn't match program_file_extension
			 else
			 ;;get file
			 file = (car last(parseString(file_path "/")))
			 
			 ;;get file extension
			 file_extension = (car last(parseString(file ".")))
			 
			 (if program_file_extension != file_extension then
			     warnText = (sprintf nil "%s is not a %s file:\nPress YES to
try again" file viewType)
			     ;;display warning box
			     retdia = (hiDisplayAppDBox
				       ?name 'xx
				       ?dboxBanner " "
				       ?dboxText warnText
				       ?buttonLayout 'YesNo )))
		     
		     ;;user gives up, so exit
		     (when !retdia		       
		       ;;if open for read, it exists
		       objId=(ddGetObj libName cellName viewName filter nil "r")		       
		       (if !objId then			 
			 ;;get object Id
			 objId = (ddGetObj libName cellName viewName)			 
			 ;;delete object
			 cmd=(sprintf nil "\\rm -Rf %s" objId~>writePath)
			 (unless (system cmd)
			   (error " ERROR - command failed: %s \n" cmd) ))
		       ;;exit
		       (return nil) ))	
	;;return the object id
	(return create_object(libName cellName viewName file_path file)) ))
