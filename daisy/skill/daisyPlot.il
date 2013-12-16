;; Functions for plotting 
;; 
;; (hiSetBindKey  "Schematics" "AltShift<Key>j" "(daisySchPlot)")
;; (hiSetBindKey  "Layout"     "AltShift<Key>j" "(daisyLePlot)" )

(printf " ... loading %s.il ...\n" "daisyPlot")

(defun daisyLePlot (@optional (destLib (strcat (getShellEnvVar "WORKAREA") "/doc/figs")))
  (let ((cw (hiGetCurrentWindow))
	(o_coords nil)
	(l_coords (hiGetMaxScreenCoords))
	(o_view (hiGetViewBBox)))
    (setq o_coords (hiGetAbsWindowScreenBBox cw))
    
    (hiResizeWindow cw 
		    (list (list 32 32) 
			  (list (car l_coords)-32 (cadr l_coords)-32)))
    (hiFlush)
    (hiZoomAbsoluteScale cw 0.9)
    (hiFlush)
    (hiZoomAbsoluteScale cw 0.9)

    (if (not destLib)
	(setq destLib (strcat (getShellEnvVar "WORKAREA") "/" cw->cellView->libName "/doc/figs")))
    (printf "%s \n" destLib)

    (setq lePlotOptions 
	  (list nil
		'area             "whole"
		'view             "layout"
		'cell             cw->cellView->cellName
		'library          cw->cellView->libName
		'plot             "cellview"
		'bBox             (hiGetViewBBox cw)
		'noteText         (strcat cw->cellView->libName ":" 
					  cw->cellView->cellName)
		'notes            nil
		'header           nil
		'plotterType      "postscript1"
		'papersize        "A3"
		'plotter          "Encapsulated Color Postscript (600 dpi)"
		'plotToFile       t
		'display          "display"
		'outputfile     (strcat (setq outFileNameTemp 
					      (strcat destLib "/" cw->cellView->viewName "_" 
						      cw->cellView->libName "_" 
						      cw->cellView->cellName))
					".eps")

		'time             "now"
		'tmpdir           "/usr/tmp"
		'copy             1
		'scale            1.0
		'center           t
		'mailto           (getShellEnvVar "EMAIL")
		'mail             t 
		'orientation      "portrait"
		'plotsize         (list 200.0 290.0)
		'offset           (list 0.0 0.0)
		'unit             "mm"
		'gridMultiple     5
		'gridSpacing      1.000000
		'gridType         "None"
		'stopLevel        32
		'startLevel       0
		'arrayDisplay     "Full"
		'instName         "instance"
		'pathCL           "no"
		'drawAxesOn       nil
		'iconsOn          nil))
    (lePlot)
    (hiFlush)
    (hiResizeWindow cw o_coords)
    (hiFlush)
    (hiZoomIn cw o_view)
    (if (isFileName (strcat outFileNameTemp ".eps"))
	(progn 
	  ;;	  (sh (sprintf nil "module rm local; module add localfirst; ps2epsi %s.eps %s.epsi" outFileNameTemp outFileNameTemp))
	  ;;  (sh (sprintf nil "module rm local; module add localfirst; convert -rotate 90 %s.eps %s.tiff" outFileNameTemp outFileNameTemp))
	  ;;  (sh (sprintf nil "module rm local; module add localfirst;
	  ;;	ps2pdf %s.eps %s.pdf" outFileNameTemp
	  ;;	outFileNameTemp))
	  (sh	(sprintf nil "daisyPlotForCadence.sh %s 0" outFileNameTemp))
	  )
		 )))


(defun daisySchPlot (@key
		     (destLib nil)
		     (cv nil)
		     )
  (let ( 
	(cw (hiGetCurrentWindow))
	(o_coords nil)
	(l_coords (hiGetMaxScreenCoords))
	(o_view (hiGetViewBBox))
	(outFileNameTemp "")
	(outFileNameConv "")
	(libName "")
	(cellName "")
	(viewName "schematic")
	)
    
    (if (not cv) 
	(progn
	  (setq libName cw->cellView->libName)
	  (setq cellName cw->cellView->cellName)
	  (setq viewName cw->cellView->viewName)
	  (setq o_coords (hiGetAbsWindowScreenBBox cw))
	  (hiResizeWindow cw
			  (list 
			   (list 32 32) 
			   (list (car l_coords)-32 (cadr l_coords)-32)))

	  (hiFlush)	  (hiZoomAbsoluteScale cw 0.9)
	  (hiFlush)	  (hiZoomAbsoluteScale cw 0.9)
	  
	  )
      (progn
	(setq libName cv->libName)
	(setq cellName cv->cellName)
	(setq viewName cv->viewName)
	)
      )
    
    (if (not destLib)
	(setq destLib 
	      (strcat 
	       (getShellEnvVar "USERAREA") 
	       "/doc/figs/" 
	       libName )))
    (printf "%s \n" destLib)
    (sh (strcat "mkdir -p " destLib))

    (setq schPlotOptions 
	  (list nil
		'library          libName
		'cell             cellName
		'view             viewName
		'viewlist         viewName
		'version          "0.0"
		'multisheet       nil
		'indexsheet       nil
		'hierarchy        nil
		'copy             1
		'plotToFile       t
		'outputfile       (strcat 
				   (setq outFileNameTemp 
					 (strcat destLib "/" 
						 viewName "_" 
						 libName "_" 
						 cellName))
					  ".eps")
		'plotter         "Encapsulated Color Postscript (600 dpi)"
		'papersize       "A4"
		'plotsize        (list 160.0 237.0)
		'offset   	 (list 10.0 10.0)
		'unit            "mm"
		'orientation     "Portrait"
		'display   	 "display"
		'tmpdir          "/tmp/"
		;;		'mailto          (getShellEnvVar "EMAIL")
		;;		'mail            t
		'header          nil
		'notes           nil
		'grid            nil
		'noteText        ""
		'scope           "/" ))
    (schPlot)

    (if (not cv)
	(progn 
	  (hiFlush)    (hiResizeWindow cw o_coords)
	  (hiFlush)    (hiZoomIn cw o_view)
	  )
      )

    (if (isFileName (strcat outFileNameTemp ".eps"))
	(progn 
	  (sh	(sprintf nil "daisyPlotForCadence.sh %s 0" outFileNameTemp))
		 
;;	  (sh (sprintf nil "module rm local; module add localfirst; convert -rotate 90 %s.eps %s.tiff" outFileNameTemp outFileNameTemp))
	;;  (sh (sprintf nil "module rm local; module add localfirst; ps2pdf %s.eps %s.pdf" outFileNameTemp outFileNameTemp)    )

	  )
      ))
  )

(defun awvSetHardCopyOptions (@optional (dummy "hmm"))
  (hardCopyOptions 
   ?hcNumCopy     1 
   ?hcPaperSize   "A4" 
   ?hcPlotterName "Encapsulated Color Postscript (600 dpi)" 
   ?hcOutputFile  (strcat (getShellEnvVar "USERAREA") 
			  "/doc/figs/simulated_libName_cellName_comment.depsc" )
   ?hcOrientation "landscape"))

(awvSetOptionValue "hcHeader" nil)
(awvSetOptionValue "hcMailLogNames" (getShellEnvVar "EMAIL"))
(awvSetOptionValue "hcOutputFile" (strcat (getShellEnvVar "USERAREA") 
					  "/doc/figs/simulated_libName_cellName_comment.depsc" ))
(awvInitWindowFunctionAdd 'awvSetHardCopyOptions)

(defun daisyAwvPlot (@optional (fileName nil) (pathName nil))
  (let ((cw (awvGetCurrentWindow))
	(o_coords nil)
	(l_coords (hiGetMaxScreenCoords))
	(fileNameX "") (pathNameX "/") (outFileNameTemp ""))
    
    (setq o_coords (hiGetAbsWindowScreenBBox cw))
    ;; (awvFatifyCurves)
    (hiResizeWindow cw 
		    (list (list 32 32) 
			  (list (car l_coords)-32 (cadr l_coords)-32)))
    (hiFlush)
    (if (not fileName)
	(setq fileNameX 
	      (strcat "simulated." 
		      (if (cw -> awvWaveInfoDpl -> title -> label) (if (equal "" cw -> awvWaveInfoDpl -> title -> label) "_" (car (parseString cw -> awvWaveInfoDpl -> title -> label))) "DUMMY")
		      "."
		      (if (cw -> awvWaveInfoDpl -> title -> label) (if (equal "" cw -> awvWaveInfoDpl -> title -> label) "_" (cadr (parseString cw -> awvWaveInfoDpl -> title -> label)))) "CELLNAME")))
    
    (setq outFileNameTemp (strcat (if pathName pathName (strcat (getShellEnvVar "USERAREA") "/doc/figs/") )
				  (if fileName fileName (if fileNameX (strcat fileNameX "")  "simulated_libName_cellName_comment"))))
    (hardCopyOptions 
     ?hcNumCopy     1 
     ?hcPaperSize   "A4" 
     ?hcPlotterName "Encapsulated Color Postscript (600 dpi)" 
     ?hcOutputFile  (strcat outFileNameTemp ".eps")
     ?hcOrientation "landscape"
     ?hcHeader nil
     ?hcMailLogNames (getShellEnvVar "EMAIL"))
    (hardCopy cw)
    
    (if (isFileName (strcat  outFileNameTemp ".eps"))
	(progn 	  
	  (sh	(sprintf nil "daisyPlotForCadence.sh %s 90" outFileNameTemp))	  
	  ;;(sh (sprintf nil "module rm local; module add localfirst; ps2pdf %s.eps  %s.pdf &" outFileNameTemp outFileNameTemp ))
	  ;;(sh (sprintf nil "module rm local; module add localfirst; convert -rotate 90 %s.eps %s.tiff" outFileNameTemp outFileNameTemp))	  
	  ))
    (hiResizeWindow cw o_coords)))
