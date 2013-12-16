(printf " ... loading awvMods.il ... \n")

(defun awvReturnSubwindow ( subWinNo winId @optional ( t_title "" ) ) 
  (let (subWinTotal)
    (if winId then
      (setq subWinTotal (car (last (awvGetSubwindowList winId ?all t))))
      (if (greaterp subWinNo subWinTotal)
	  (for subWindow (subWinTotal+1) subWinNo
	       (awvAddSubwindow winId)))
      (awvSetCurrentSubwindow winId subWinNo)
      (awvDisplaySubwindowTitle winId t_title)
      else
      nil) ) )

(defun awvRemoveAllGrids ()			 
  (awvSetAllGridDisplay nil)  
  (setq gridOnInWaveform nil))

(defun awvToggleAllGrids ()			 
  (awvSetAllGridDisplay (if (boundp 'gridOnInWaveform) (setq gridOnInWaveform (not gridOnInWaveform)) (setq gridOnInWaveform t))))


(defun awvSetAllGridDisplay (status)		 
  (let ((winId (awvGetCurrentWindow)))
    (when winId
      ;; Set the grid display option for every subwindow.
      (foreach subWinNo (awvGetSubwindowList winId)
	       (awvDisplayGrid winId (and status t) 
			       ?subwindow subWinNo))
      t)))

(defun awvToggleSubwindowDisplayStatus (subwinNo)
  (let ((winId (awvGetCurrentWindow)))
    (when winId
      ;; It has to be a waveform window
      (if (member subwinNo (awvGetSubwindowList winId ?all t))
	  (if (member subwinNo (awvGetOnSubwindowList winId ?all t))
	      (awvSetDisplayStatus winId nil ?subwindow subwinNo)
	    (progn	      
	      (awvSetDisplayStatus winId  t ?subwindow subwinNo)
	      (awvSetCurrentSubwindow winId subwinNo)))	
	(for subWinNumber 1 subwinNo
	     (if (not (member subWinNumber (awvGetSubwindowList winId ?all t)))
		 (progn 
		   (awvAddSubwindow winId)
		   (if (not (equal subwinNo subWinNumber))
		       (awvSetDisplayStatus winId  nil ?subwindow subWinNumber)
		     (progn 
		       (awvSetDisplayStatus winId t ?subwindow subwinNo)
		       (awvSetCurrentSubwindow winId subwinNo))))))))))

(defun awvFatifyCurves ()	
  ;; Takes the curves of a subwindow and uses the fatter line for 
  ;; plotting and documentation purposes.  
  (let ((l_penStyle (list 52 53 54 55 56 57 58 59))
	(winId (awvGetCurrentWindow))
	waveInfo currentPen)
    (when winId                                             ; It has to be a waveform window
      (graphicsOff)
      (foreach subWinNo (awvGetSubwindowList winId ?all t)
	       
	       (foreach wave (car (awvGetWaveNameList winId ?subwindow subWinNo))
			
			;; this is a private function, which returns a list of information about
			;; the status of a waveform. In order it returns:
			;; (expr stripNumber yNumber penIndex plotSymbol component)
			(setq waveInfo (awviGetCurveInfo winId wave ?subwindow subWinNo))
			
			;; if we've reached the end of the list of pens, start at the beginning again
			(unless currentPen (setq currentPen l_penStyle))
			
			;; replace the penIndex in the waveInfo with the current fat pen
			(rplaca (nthcdr 3 waveInfo) (car currentPen))
			
			;; call a private function with the modified waveInfo - have
			;; to supply some additional arguments as well, hence the list splicing
			
			(apply 'awviSetCurveInfo `(,winId ,wave ,@waveInfo ?subwindow ,subWinNo))
			;; and finally move on to the next pen
			
			(setq currentPen (cdr currentPen)) ))
      (graphicsOn) )))

(defun daisyDistributeCurves ()	  
  (let ((winId (awvGetCurrentWindow))
	(stripPointer 0) (waveInfo nil))
    (when winId                                             ; It has to be a waveform window
      (graphicsOff)
      (foreach subWinNo (awvGetSubwindowList winId ?all t)	       
	       (setq stripPointer 1)
	       (foreach wave (car (awvGetWaveNameList winId ?subwindow subWinNo))
			;; this is a private function, which returns a list of information about
			;; the status of a waveform. In order it returns:
			;; (expr stripNumber yNumber penIndex plotSymbol component)
			(setq waveInfo (awviGetCurveInfo winId wave ?subwindow subWinNo))
			(rplaca (cdr waveInfo) stripPointer)
			;; call a private function with the modified waveInfo - have
			;; to supply some additional arguments as well, hence the list splicing
			
			(apply 'awviSetCurveInfo `(,winId ,wave ,@waveInfo ?subwindow ,subWinNo))
			(if (equal stripPointer 12) (setq stripPointer 0))
			(setq stripPointer (plus stripPointer 1))))
      (graphicsOn) )))

(defun daisyAwvAxeThem ()
  ;; Takes the curves of a subwindow and uses the fatter line for 
  ;; plotting and documentation purposes.  
  (let ((l_penStyle (list 51 52 53 54 55 56 57 58))
	(winId (awvGetCurrentWindow))
	waveInfo currentPen)
    (when winId                                             ; It has to be a waveform window
      (graphicsOff)
      (foreach subWinNo (awvGetSubwindowList winId ?all t)	       
	       (foreach wave (car (awvGetWaveNameList winId ?subwindow subWinNo))
			
			;; this is a private function, which returns a list of information about
			;; the status of a waveform. In order it returns:
			;; (expr stripNumber yNumber penIndex plotSymbol component)
			(setq waveInfo (awviGetCurveInfo winId wave ?subwindow subWinNo))			
			(rplaca (nthcdr 2 waveInfo) 1)
			;; call a private function with the modified waveInfo
			(apply 'awviSetCurveInfo `(,winId ,wave ,@waveInfo ?subwindow ,subWinNo))))
      (graphicsOn) )))

/* Special settings */

(awvSetOptionValue "cursorPrecision" 8)
(awvSetOptionValue "dateStamp" t)
(awvSetOptionValue "displayGrids" nil)

