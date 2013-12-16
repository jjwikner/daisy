; Log to CIW

(printf "Defining Special Calculator Functions HI.\n")

; Diable write protection if enabled.
(setq daisyWriteProtectStatus (status writeProtect))
(sstatus writeProtect nil)

;===============================================================================
; Menus interfaces for specials functions.
; ERROR. Note the size of these windows is crap! Because its does not say
;  anywhere what the numbers do!

(defun daisyCreateFormOneParameter ()		  ;_Dec 22 04 wikner 300
  (let ( (fieldList nil) )
    (setq fieldList 
	  (cons
	   (list
	    (ahiCreateStringField
	     ?name 'parameter
	     ?prompt "parameter"
	     ?value "0"
	     ) (list 0 0) (list 300 26) 80)	   
	   fieldList ))
    (calCreateSpecialFunctionsForm 
     'daisyFormOneParameter fieldList) ))

(defun daisyCreateFormTwoParameters ()
  (let ( (fieldList nil) )
    (setq fieldList 
   (cons
    (list
     (ahiCreateStringField
      ?name 'parameter0
      ?prompt "parameter0"
      ?value "0"
      ) 0:0 300:26 80 )
    fieldList ))
    (setq fieldList 
	  (cons
	   (list
	    (ahiCreateStringField
	     ?name 'parameter1
	     ?prompt "parameter1"
	     ?value "0"
	     ) 0:26 300:26 80 )
	   fieldList ))
    (calCreateSpecialFunctionsForm 
     'daisyFormTwoParameters fieldList) ))

;===============================================================================
; Define callback procedures for custom special calculator functions.

;-------------------------------------------------------------------------------
; General.



(defun cjStaircaseCB ()
  (calSpecialFunctionInput 'staircaseWave nil) )

(defun cjCreateFormSampToEnd ()
  (let ( (fieldList nil) )
    fieldList=(cons
               (list
                (ahiCreateStringField
                 ?name 'tstart
                 ?prompt "tstart"
                 ?value "0"
                 ) 0:0 300:26 80 )
               fieldList)
    fieldList=(cons
               (list
                (ahiCreateCyclicField
                 ?name 'sampType
                 ?choices (list "linear" "log")
                 ?prompt "Type"
                 ?value "linear"
                 ) 0:26 300:26 80 )
               fieldList )
    fieldList=(cons
               (list
                (ahiCreateStringField
                 ?name 'timeStep
                 ?prompt "tstep"
                 ?value "10n"
                 ) 0:2*26 300:26 80 )
               fieldList )
    (calCreateSpecialFunctionsForm 'cjFormSampToEnd fieldList) ))

(defun cjSampToEndCB ()
  (calCreateSpecialFunction
   ?formSym 'cjFormSampToEnd
   ?formInitProc 'cjCreateFormSampToEnd
   ?formTitle "sample"
   ?formCallback "(calSpecialFunctionInput  'sampToEnd
      '(tstart sampType timeStep) )" ))



(defun daisyBitCB ()
  (calCreateSpecialFunction 
   ?formSym      'daisyFormOneParameter
   ?formInitProc 'daisyCreateFormOneParameter
   ?formTitle    "Enter: Bit number"
   ?formCallback "(calSpecialFunctionInput  'bit '(parameter) )" ) )

(defun daisyBin2decCB ()
  (calSpecialFunctionInput 'bin2dec nil)
  (hiRaiseWindow (hiGetCIWindow))
  (hiGetAttention)
  (printf "Edit calculator buffer to format,\n")
  (printf "bin2dec(bo, b1, b2, ...)\n") )

(defun daisyClipByIndexCB ()
  (calCreateSpecialFunction
   ?formSym 'daisyFormTwoParameters
   ?formInitProc 'daisyCreateFormTwoParameters
   ?formTitle "Enter: clip by index from Start to Stop"
   ?formCallback "(calSpecialFunctionInput 'clipByIndex '(parameter0 parameter1))" ))

(defun daisyClipPSCB ()
  (hiRaiseWindow (hiGetCIWindow ))
  hiGetAttention()
  (printf "Use clip() fn. Edit calculator buffer and change,\n")
  (printf "clip(...) --> clipPS(...)\n") )

(defun daisyCreateFormCrossings ()
  (let ( (fieldList nil) )
    fieldList=(cons 
	       (list 
		(ahiCreateStringField 
		 ?name 'th
		 ?prompt "threshold"
		 ?value "0"
		 ) 0:0 300:26 80 )
	       fieldList)
    fieldList=(cons 
	       (list 
		(ahiCreateCyclicField 
		 ?name 'direc
		 ?choices (list "rising" "either" "falling")
		 ?prompt "direction"
		 ?value "either"
		 ) 0:26 300:26 80 ) 
	       fieldList )
    fieldList=(cons 
	       (list 
		(ahiCreateStringField 
		 ?name 'start
		 ?prompt "start"
		 ?value "0"
		 ) 0:2*26 300:26 80 )
	       fieldList )
    fieldList=(cons 
	       (list 
		(ahiCreateStringField 
		 ?name 'tau
		 ?prompt "tau"
		 ?value "0"
		 ) 0:3*26 300:26 80 )
	       fieldList)
    (calCreateSpecialFunctionsForm 'daisyFormCrossings fieldList) ))

(defun daisyCrossingsCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormCrossings
   ?formInitProc 'daisyCreateFormCrossings
   ?formTitle "crossings"
   ?formCallback "(calSpecialFunctionInput  'crossings
      '(th direc start tau) )" ))

(defun daisyDeriv2CB ()
  (calSpecialFunctionInput 'deriv2 nil) )

(defun daisyDftPSCB ()
  (hiRaiseWindow (hiGetCIWindow ))
  (hiGetAttention)
  (printf "Use dft() fn. Edit calculator buffer and change,\n")
  (printf "dft(...) --> dftPS(...)\n"))

(defun daisyGetFamilyWaveCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormOneParameter
   ?formInitProc 'daisyCreateFormOneParameter
   ?formTitle "Enter: base wave number (integer >=0)"
   ?formCallback "(calSpecialFunctionInput  'getFamilyWave '(parameter) )"))

(defun daisyGtCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormOneParameter
   ?formInitProc 'daisyCreateFormOneParameter
   ?formTitle "Enter: > Threshold"
   ?formCallback "(calSpecialFunctionInput  'gt '(parameter) )" ))

(defun daisyHistogramCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormOneParameter
   ?formInitProc 'daisyCreateFormOneParameter
   ?formTitle "Enter: Histogram Slices (>1)"
   ?formCallback "(calSpecialFunctionInput  'histogramSlice '(parameter) )" ))

(defun daisyLog2CB ()
  (calSpecialFunctionInput 'log2 nil) )

(defun daisyLtCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormOneParameter
   ?formInitProc 'daisyCreateFormOneParameter
   ?formTitle "Enter: < Threshold"
   ?formCallback "(calSpecialFunctionInput  'lt '(parameter) )" ))

(defun daisyMark2spaceCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormOneParameter
   ?formInitProc 'daisyCreateFormOneParameter
   ?formTitle "Enter: Crossing Threshold"
   ?formCallback "(calSpecialFunctionInput  'mark2space '(parameter) )" ))

(defun daisyMaxXCB ()
  (calSpecialFunctionInput 'maxX nil) )

(defun daisyMinXCB ()
  (calSpecialFunctionInput 'minX nil) )

(defun daisyOverlayPeriodCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormTwoParameters
   ?formInitProc 'daisyCreateFormTwoParameters
   ?formTitle "Enter: Overlay Start and Period"
   ?formCallback "(calSpecialFunctionInput 
      'overlayPeriod '(parameter0 parameter1)    )"  ))

(defun daisyPkDetCB ()
(calSpecialFunctionInput 'pkDet nil))

(defun daisyCreateFormRunDelay ()
  (let ( (fieldList nil) )
    fieldList=(cons 
	       (list 
		(ahiCreateStringField 
		 ?name 'th
		 ?prompt "threshold"
		 ?value "0"
		 ) 0:0 300:26 80)
	       fieldList)
    fieldList=(cons 
	       (list 
		(ahiCreateCyclicField 
		 ?name 'direc
		 ?choices (list "rising" "either" "falling")
		 ?prompt "direction"
		 ?value "either"
		 ) 0:26 300:26 80)
	       fieldList)
    fieldList=(cons 
	       (list 
		(ahiCreateStringField 
		 ?name 'start
		 ?prompt "start"
		 ?value "0"
		 ) 0:2*26 300:26 80 )
	       fieldList)
    (calCreateSpecialFunctionsForm 'daisyFormRunDelay fieldList) ))

(defun daisyRunDelayCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormRunDelay
   ?formInitProc 'daisyCreateFormRunDelay
   ?formTitle "runDelay"
   ?formCallback "(calSpecialFunctionInput  'runDelay
      '(STACK th direc start) )"))

(defun daisyScaleXCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormOneParameter
   ?formInitProc 'daisyCreateFormOneParameter
   ?formTitle "Enter: Value of scale x-axis by"
   ?formCallback "(calSpecialFunctionInput  'scaleX '(parameter) )"))

(defun daisySgnCB ()
  (calSpecialFunctionInput 'sgn nil))

(defun daisySigmaCB ()
  (calSpecialFunctionInput 'sigma nil) )

(defun daisySkipPtsCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormOneParameter
   ?formInitProc 'daisyCreateFormOneParameter
   ?formTitle "Enter: No pts to skip"
   ?formCallback "(calSpecialFunctionInput  'skipPts '(parameter) )" ))

(defun daisyCreateFormTime2Freq ()
  (let ( (fieldList nil) )
    fieldList=(cons 
	       (list 
		(ahiCreateStringField 
		 ?name 'th
		 ?prompt "threshold"
		 ?value "0"
		 ) 0:0 300:26 80 )
	       fieldList)
    fieldList=(cons 
	       (list 
		(ahiCreateCyclicField 
		 ?name 'direc
		 ?choices (list "rising" "either" "falling")
		 ?prompt "direction"
		 ?value "either"
		 ) 0:26 300:26 80 )
	       fieldList)
    fieldList=(cons 
	       (list 
		(ahiCreateStringField 
		 ?name 'start
		 ?prompt "start"
		 ?value "0"
		 ) 0:2*26 300:26 80 )
	       fieldList)
    (calCreateSpecialFunctionsForm 'daisyFormTime2Freq fieldList) ))

(defun daisyTime2freqCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormTime2Freq
   ?formInitProc 'daisyCreateFormTime2Freq
   ?formTitle "time2freq"
   ?formCallback "(calSpecialFunctionInput  'time2freq
      '(th direc start) )" ))

(defun daisyValuePSCB ()
  (hiRaiseWindow (hiGetCIWindow ))
  hiGetAttention()
  (printf "Use value() fn. Edit calculator buffer and change,\n")
  (printf "value(...) --> valuePS(...)\n") )

(defun daisyXValCB ()
  (calSpecialFunctionInput 'xVal nil) )

;; -------------------------------------------------------------------------------
;; Data entry.
(defun daisyTableCB ()
  (hiRaiseWindow (hiGetCIWindow ))
  (hiGetAttention)
  (printf "Edit calculator buffer to format,\n")
  (printf "table(x, x0, y0, ..., xn, yn)\n"))

(defun daisyWaveCB ()
  (hiRaiseWindow (hiGetCIWindow ))
  (hiGetAttention)
  (printf "Edit calculator buffer to format,\n")
  (printf "wave(x0, y0, ..., xn, yn)\n"))

;; -------------------------------------------------------------------------------
;; Data storage.
(defun daisyDumpWave2fileCB ()
  (calCreateSpecialFunction 
   ?formSym 'daisyFormOneParameter
   ?formInitProc 'daisyCreateFormOneParameter
   ?formTitle "Enter: \"fileName\""
   ?formCallback "(calSpecialFunctionInput  'dumpWave2file '(parameter) )" ))

;; -------------------------------------------------------------------------------
;; Impedance measurement.

(defun daisyRinSCB ()
  (calSpecialFunctionInput 'RinS '(STACK)))

(defun daisyXinSCB ()
  (calSpecialFunctionInput 'XinS '(STACK)))

(defun daisyCinSCB ()
  (calSpecialFunctionInput 'CinS '(STACK)))

(defun daisyLinSCB ()
  (calSpecialFunctionInput 'LinS '(STACK)))

(defun daisyRinPCB ()
  (calSpecialFunctionInput 'RinP '(STACK)))

(defun daisyXinPCB ()
  (calSpecialFunctionInput 'XinP '(STACK)))

(defun daisyCinPCB ()
  (calSpecialFunctionInput 'CinP '(STACK)))

(defun daisyLinPCB ()
  (calSpecialFunctionInput 'LinP '(STACK)))

(defun daisyAcVSWRCB ()
  (calCreateSpecialFunction 
    ?formSym 'daisyFormOneParameter
    ?formInitProc 'daisyCreateFormOneParameter
    ?formTitle "Enter: Drive Zo"
    ?formCallback "(calSpecialFunctionInput  'acVSWR '(STACK parameter) )"))

(defun daisyZportCB ()
  (calCreateSpecialFunction 
    ?formSym 'daisyFormOneParameter
    ?formInitProc 'daisyCreateFormOneParameter
    ?formTitle "Enter: Port source/load impedance"
    ?formCallback "(calSpecialFunctionInput  'Zport '(parameter) )" ))

;-------------------------------------------------------------------------------
; Noise.
(defun daisyVN2CB ()
  (calCalcInput 'expression "VN2()"))

(defun daisyVNINCB ()
  calCalcInput('expression "VNIN()"))

(if (equal (getShellEnvVar "CDSDATABASE") "cds") (caliCreateCalcForms))
(caliRegisterSpecialFunctions)

;; Register each function menu name & callback.
(foreach fnToReg '(
		   ;; General.
		   ("igwLib: bit" daisyBitCB )
		   ("igwLib: bin2dec" daisyBin2decCB )
		   ("igwLib: clipByIndex" daisyClipByIndexCB )
		   ("igwLib: clipPS" daisyClipPSCB )
		   ("igwLib: crossings" daisyCrossingsCB )
		   ("igwLib: deriv2" daisyDeriv2CB )
		   ("igwLib: dftPS" daisyDftPSCB )
		   ("igwLib: getFamilyWave" daisyGetFamilyWaveCB )
		   ("igwLib: gt" daisyGtCB )
		   ("igwLib: histogram" daisyHistogramCB )
		   ("igwLib: log2" daisyLog2CB )
		   ("igwLib: lt" daisyLtCB )
		   ("igwLib: mark2space" daisyMark2spaceCB )
		   ("igwLib: maxX" daisyMaxXCB )
		   ("igwLib: minX" daisyMinXCB )
		   ("igwLib: overlayPeriod" daisyOverlayPeriodCB )
		   ("igwLib: pkDet" daisyPkDetCB )
		   ("igwLib: runDelay" daisyRunDelayCB )
		   ("igwLib: scaleX" daisyScaleXCB )
		   ("igwLib: sgn" daisySgnCB )
		   ("igwLib: sigma" daisySigmaCB )
		   ("igwLib: skipPts" daisySkipPtsCB )
		   ("igwLib: time2freq" daisyTime2freqCB )
		   ("igwLib: valuePS" daisyValuePSCB )
		   ("igwLib: xVal" daisyXValCB )
		   ;; Data entry.
		   ("igwLib: table" daisyTableCB )
		   ("igwLib: wave" daisyWaveCB )
		   ;; Data storage.
		   ("igwLib: dumpWave2file" daisyDumpWave2fileCB )
		   ;; Impedance measurement.
		   ("igwLib: RinS" daisyRinSCB )
		   ("igwLib: XinS" daisyXinSCB )      
		   ("igwLib: CinS" daisyCinSCB )      
		   ("igwLib: LinS" daisyLinSCB )      
		   ("igwLib: RinP" daisyRinPCB )
		   ("igwLib: XinP" daisyXinPCB )
		   ("igwLib: CinP" daisyCinPCB )
		   ("igwLib: LinP" daisyLinPCB )
		   ("igwLib: acVSWR" daisyAcVSWRCB )
		   ("igwLib: Zport" daisyZportCB )
		   ;; Noise.
		   ("igwLib: VN2" daisyVN2CB )
		   ("igwLib: VNIN" daisyVNINCB )
                   ("cj: Staircase" cjStaircaseCB )
                   ("cj: SampToEnd" cjSampToEndCB )
                 )
	 ;;(printf "  Register calc special fn: %s\n" car(fnToReg))
	 (calRegisterSpecialFunction fnToReg))

;===============================================================================

;-------------------------------------------------------------------------------
; Reset writeProtect status.
(sstatus writeProtect daisyWriteProtectStatus)
