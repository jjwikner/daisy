;;-------------------------------------------------------------------------------
;; Calculator Special Functions Definitions.
;; Log to CIW
(printf "Defining Special Calculator Functions.\n")

;; Diable write protection if enabled.
(setq daisyWriteProtectStatus status(writeProtect))
(sstatus writeProtect nil)

;;===============================================================================
;; Common sub functions.

/*------------------------------------------------------------------------------
	Linear interpolation function.
	y-y0   y1-y0
	---- = -----
	x-x0   x1-x0
*/

(defun yCrossValue (x x0 y0 x1 y1)		  ;_Oct 29 04 wikner 123
  (if (equal x0 x1)
     y0
     y0 + (x-x0) * (y1-y0)/(x1-x0) ))

(defun xCrossValue (y x0 y0 x1 y1)		  ;_Oct 29 04 wikner 0
  (if (equal y0 y1) 
     x0
     x0 + (y-y0) * (x1-x0)/(y1-y0) ) )

/*------------------------------------------------------------------------------
	Display waveform.
	Print the details and value of a waveform in the CIW. The data is printed

Arg:
x		number/drwave/family
digits		number
                Default 4.
		Number of significant digits printed.
file		string/nil
		Default nil
		If set to nil results are displayed in CIW in seqential order. If a file is specified the results are save to the file in tabular format.
Ret:
		number/drwave/family
*/

(defun showWaveform (x @optional (digits 4) (file nil))  
  (let ( (xVec nil)
	 (yVec nil)
	 (fp nil) )    
    ;; Ensure daisyCSFCountShowWaveform exists with correct initialisation value.
    (unless  (boundp 'daisyCSFCountShowWaveform)
      (setq daisyCSFCountShowWaveform -1))    
    (cond 
     ( (numberp x)
       x )
     ( (drIsWaveform x)					; Set number of digits.
       (aelPushSignifDigits digits)
       ;; Get lengths of data vectors.
       (setq xVec (drGetWaveformXVec x))
       (setq yVec (drGetWaveformYVec x))
       ;; Open file is required.
       (when daisyCSFCountShowWaveform<=0
	 declare( daisyCSFShowWaveformDataY[(drVectorLength xVec)] )
	 declare( daisyCSFShowWaveformDataX[(drVectorLength xVec)] )
	 (for i 0 (drVectorLength xVec)-1
	      daisyCSFShowWaveformDataX[i]=aelEngNotation((drGetElem xVec i))
	      daisyCSFShowWaveformDataY[i]=nil ))
       (for i 0 (drVectorLength xVec)-1
	    daisyCSFShowWaveformDataY[i] =  
	    (append1
	     daisyCSFShowWaveformDataY[i]
	     aelEngNotation((drGetElem yVec i)) ) )
       ;; Increment count.
       (when daisyCSFCountShowWaveform>=0
	 daisyCSFCountShowWaveform++ )
       ;; Reset number of digits.
       (aelPopSignifDigits)     
       ;; Return
       x )
     ( (famIsFamily x)
       ;; Set global count variable.
       daisyCSFCountShowWaveform=0
       (famMap 'showWaveform x digits file)
       daisyCSFCountShowWaveform=-1 )
     ( t
       (artError "showWaveform: can't handle showWaveform%L\n"
		 (list x digits file) )))
    
    ;; Output data if required.
    (when daisyCSFCountShowWaveform<0
      ;; Open file if required.
      (when file
	(setq fp (outfile file "w"))
	;; Start tilte line with column comments.
	(if fp 
	    (fprintf fp "%10s" "xVec")
	  (printf "%10s" "xVec"))
	;; Comment each column if a family.
	(when (famIsFamily x)
	      xVec=(drGetWaveformXVec x)
	      
	      (for i 0 (drVectorLength xVec)-1
		   (if fp
		       (fprintf fp "\t%10s" aelEngNotation((drGetElem xVec i)))
		     (printf "\t%10s" aelEngNotation((drGetElem xVec i)))) ))
	;; Terminate title line.
	(if fp
	    (fprintf fp "\n")
	  (printf "\n"))
	
	;; Dump data to file.
	(for i 0 length(daisyCSFShowWaveformDataX)-1
	     (if fp 
		 (fprintf fp "%10s" daisyCSFShowWaveformDataX[i])
	       (printf "%10s" daisyCSFShowWaveformDataX[i]) )
	     (foreach y daisyCSFShowWaveformDataY[i]
		      (if fp 
			  (fprintf fp "\t%10s" y)
			(printf "\t%10s" y)) )
	     (if fp
		 (fprintf fp "\n")
	       (printf "\n")) )
	;; Close file if open and edit it to display values.
	(when file
	  (close fp)
	  (edit (strcat 
		 (getWorkingDir) 
		 "/" 
		 (car (last (parseString file "/")))))) )
      0 )))

;===============================================================================
; General.


/*------------------------------------------------------------------------------
  Same as built in sample function except that it samples to end of wave

Arg
  waveIn      Input waveform
  sampStart   Time when samlling starts
  sampType    "linear" or "log"
  sampStep    Sampling period

Ret
  sampled wave

*/

procedure(sampToEnd(waveIn, sampStart, sampType, sampStep)
  let((sampEnd)
    when(drIsWaveform(waveIn) 
      sampEnd = maxX(waveIn)
      sample(waveIn, sampStart, sampEnd, sampType, sampStep)
    ) ;when
  ) ; let
) ;procedure



/*------------------------------------------------------------------------------
  Creates a staircase waveform of the data points available.
  Intended for use on a sampled waveform.

Arg
  waveIn    Input waveform

Ret
  sampled waveform

*/

(procedure staircaseWave(waveIn)
  let( (vLen xArrayIn yArrayIn xArrayOut yArrayOut (vIndex 1) thisX thisY waveOut)
    when(drIsWaveform(waveIn) 
      xArrayIn  = drGetWaveformXVec(waveIn)
      yArrayIn  = drGetWaveformYVec(waveIn)
      vlen      = drVectorLength(xArrayIn)
      thisX     = drGetElem(xArrayIn 0)
      thisY     = drGetElem(yArrayIn 0)
      prevY     = thisY
      xArrayOut = drCreateVec(drGetWaveformXType(waveIn) list(thisX)) 
      yArrayOut = drCreateVec(drGetWaveformYType(waveIn) list(thisY)) 
      while((vIndex < vlen)
        thisX =  drGetElem(xArrayIn vIndex)
        thisY =  drGetElem(yArrayIn vIndex)
        drAddElem(xArrayOut thisX) 
        drAddElem(yArrayOut prevY) 
        drAddElem(xArrayOut thisX) 
        drAddElem(yArrayOut thisY) 
        prevY = thisY
        vIndex++ 
      ) ;while vIndex
      waveOut = drCreateWaveform(xArrayOut yArrayOut) 
    ) ;  when waveIn
  ) ; let
) ; procedure



/*------------------------------------------------------------------------------
	Binary to decimal: bin2dec(b0, b1, ...)
Arg:
b		number/drwave/family
		Bits in assending exponent, e.g. lsb first.
Ret:
		number
		Decimal equivalent of binary
*/

(defun bin2dec (@rest bits)
  (let (  (xVecNew nil)
	  (yVecNew nil) 
	  (n 0)
	  (newWave nil) ) 
    
    ;; Start result vectors.
    (setq xVecNew (drGetWaveformXVec (car bits)))
    (setq yVecNew (drCreateVec 'intlong (drVectorLength xVecNew)))
    
    (for i 0 (drVectorLength xVecNew)-1
	 ;; Reset count to 0.
	 (setq n 0)
	 (for b 0 (length bits)-1
	      ;; printf("i=%d b=%d n=%g\n" i b n)
	      (when (or (greaterp (abs (drGetElem (drGetWaveformYVec (nth b bits)) i)) 0.5) 
			(equal (abs (drGetElem (drGetWaveformYVec (nth b bits)) i)) 0.5))
		(setq n n+2**b) ))
	 ;; Set decimal value of bits.
	 (drSetElem yVecNew i n) )
    
    ;; Create new waveform.
    (setq newWave (drCreateEmptyWaveform))
    
    (drPutWaveformXVec newWave xVecNew)
    (drPutWaveformYVec newWave yVecNew)
    
    ;; Return waveform.
    newWave     ) )

(defun bus2dec (bus)
  (let (  (xVecNew nil)
	  (yVecNew nil) 
	  (n 0)
	  (newWave nil) ) 
    
    ;; Start result vectors.
    (setq xVecNew (drGetWaveformXVec bus))
    (setq yVecNew (drCreateVec 'intlong (drVectorLength xVecNew)))
    
    (for i 0 (drVectorLength xVecNew)-1
	 ;; Reset count to 0.
	 (setq n 0)
	 (for busIndex 0 (length bits)-1
	      ;; printf("i=%d b=%d n=%g\n" i b n)
	      (when (or (greaterp (abs (drGetElem (drGetWaveformYVec (nth busIndex bits)) i)) 0.5) 
			(equal (abs (drGetElem (drGetWaveformYVec (nth busIndex bits)) i)) 0.5))
		(setq n n+2**busIndex) ))
	 ;; Set decimal value of bits.
	 (drSetElem yVecNew i n) )
    
    ;; Create new waveform.
    (setq newWave (drCreateEmptyWaveform))
    
    (drPutWaveformXVec newWave xVecNew)
    (drPutWaveformYVec newWave yVecNew)
    
    ;; Return waveform.
    newWave     ) )

/*------------------------------------------------------------------------------
	Binary bit of function: bit(x b)
Arg:
x		number/drwave/family
b		number
Ret:
		number
		return the b bit of x
		e.g. x=6  == ...0110
		                3210 bit b
		bit(x 0)=0
		bit(x 1)=1
		bit(x 2)=1
		bit(x 3)=0 ...
*/

(defun bit (x b)
  (cond 
   ( ( numberp x) || (drIsWaveform x)
     2.0*(int(x/2**b)/2.0-int(int(x/2**b)/2.0)))
   
   ( (famIsFamily x)
     (famMap 'bit x b) )
   ( t
     (artError "bit: can't handle bit%L\n" (list x b)) )))

/*------------------------------------------------------------------------------
	Clip by x index from start to stop: clipByIndex(x start stop)

	As clip(x start stop), but start and stop are given as the vector
index's using zero-base.

Arg:
x		number/drwave/family
start		number
stop		number
Ret:
		number/drwave/family
*/
(defun clipByIndex (x start stop)
  (let  (xVec yVec xVecNew yVecNew step)
       (cond 
	    ( (numberp x)
        x
      )
      ( (drIsWaveform x)
        ; Get vectors.
        xVec=(drGetWaveformXVec x)
        yVec=(drGetWaveformYVec x)

        ; Create new vector lists, using start point.
        xVecNew=(tconc nil (drGetElem xVec start))
        yVecNew=(tconc nil (drGetElem yVec start))

        ; Build new vector.
        (if  start<stop then
          step=1
        else
          step=-1
        )
        i=start
        (while  i!=stop
          i=i+step
          (tconc xVecNew (drGetElem xVec i))
          (tconc yVecNew (drGetElem yVec i))
        ) ;for

        ; Create new waveform
        newWave=drCreateEmptyWaveform()
        (drPutWaveformXVec newWave (drCreateVec drType(xVec) car(xVecNew)))
        (drPutWaveformYVec newWave (drCreateVec drType(yVec) car(yVecNew)))
        newWave
      )
      ( (famIsFamily x)
        (famMap 'clipByIndex x start stop)
      )
      ( t
        (artError "clipByIndex: can't handle clipByIndex%L\n" (list x start stop))
      )
    )
  ) ;let
) ;proc


/*------------------------------------------------------------------------------
	Clip of x at start to stop: clipPS(x start stop)

	As clip(x start stop), but works when given families of curves, where
start and/or stop are waves.

Arg:
y		number/drwave/family
x		number/drwave
Ret:
		number/drwave/family
*/
(defun clipPS (x start stop)
  (let  (yVecStart yVecStop ans)
    (cond 
      ( (numberp x)
        x
      )
      ( (drIsWaveform x)
        (cond 
          ( (numberp start) && (numberp stop)
            ; Apply function.
            clip(x start stop)
          )
          ( (drIsWaveform start) && (drIsWaveform stop)
            ; start & stop are drWaveID, so its a family of curves.
            yVecStart=(drGetWaveformYVec start)
            yVecStop=(drGetWaveformYVec stop)
            ; Apply function.
            ans=clip(x (drGetElem yVecStart daisyCSFCountClipPS)
              (drGetElem yVecStop daisyCSFCountClipPS)
            )
            daisyCSFCountClipPS++
            ans
          )
          ( (drIsWaveform start) && (numberp stop)
            ; start is a drWaveID, so its a family of curves.
            yVecStart=(drGetWaveformYVec start)
            ; Apply function.
            clip(x (drGetElem yVecStart daisyCSFCountClipPS++) stop)
          )
          ( (numberp start) && (drIsWaveform stop)
            ; start is a drWaveID, so its a family of curves.
            yVecStop=(drGetWaveformYVec stop)
            ; Apply function.
            clip(x start (drGetElem yVecStop daisyCSFCountClipPS++))
          )
          ( t
            (artError "clipPS: can't handle clipPS%L with curve family\n"
              (list x start stop)
            )
          )
        ) ;cond
      )
      ( (famIsFamily x)
        ; x could be a drWaveID, so set the global daisyCSFCountClipPS.
        daisyCSFCountClipPS=0
        (famMap 'clipPS x start stop)
      )
      ( t
        (artError "clipPS: can't handle clipPS%L\n" (list x start stop))
      )
    )
  ) ;let
) ;proc


/*------------------------------------------------------------------------------
	crossings: crossings(x [[[[th] direc] start] tau])
	List values of independent axis values where x crosses the threshold 
        'th' in the direction 'direc'. The threshold maybe a number or dependent 
        waveform of the same length as x. Such a varying threshold allows a 
        moving threshold to be used. The direction allows rising, either or 
        falling edges to be found.

Arg:
x		number/drwave/family
th		number/drwave
		Optional after x.
direc		string
		Optional after th.
start		number
		Optional after direc.
tau		number
		Optional after start.
Ret:
		number/drwave/family
*/
(defun crossings (x @optional (th 0.0) (direc "either") (start 0.0) (tau 0.0))
  (let  (xVec yVec xVecNew yVecNew lenXVec 
	      (is 0) (xStart nil) xValue j (countBasie 0)
	      newWave)
    (cond 
     ( (numberp x) x)
     ( (drIsWaveform x)
       ;; Subtract threshold
       (setq  x x-th)
       ;; Get vectors.
       (setq xVec (drGetWaveformXVec x))
       (setq yVec (drGetWaveformYVec x))
       
       ;; Length of vectors.
       (setq lenXVec (drVectorLength xVec))
       
       ;; Check length of vector & start less than end time.
       (if  (and (greaterp lenXVec 1) (lessp  start (drGetElem xVec lenXVec-1))) then
	 ;; Speed up direction checking.
	 (when direc=="either"  direc='either)
	 (when direc=="rising"  direc='rising)
	 (when direc=="falling" direc='falling)
	 
	 ;; Create new vector lists.
	 (setq xVecNew (ncons nil))
	 (setq yVecNew (ncons nil))
	 
	 ;; Scan forward to start.
	 (while  (greaterp start (drGetElem xVec is)) 		   
	   is++)
	 ;; Scan waveform quickly for crossing, then process details.
	 (setq j is+1)
	 (for i is lenXVec-2
	      
	      (when  (or (lessp (drGetElem yVec i)*(drGetElem yVec j) 0.0 )
			 (and (equal (drGetElem yVec i) 0.0)  (not (equal (drGetElem yVec j) 0.0))))
		
		;; Check direction.
		(when  (or direc=='either
			   (and direc=='rising (greaterp (drGetElem yVec j) (drGetElem yVec i)))
			   (and direc=='falling (lessp (drGetElem yVec j) (drGetElem yVec i))))
		  
		  ;; Linear interpolate crossing point.
		  (setq xValue (xCrossValue 0.0
					    (drGetElem xVec i) (drGetElem yVec i)
					    (drGetElem xVec j) (drGetElem yVec j)))
		  
		  ;; Is this the 1st crossing point.
		  (unless xStart xStart=xValue)
		  
		  ;; If tau>0.0 then subtract an integer multiple of tau.
		  (when (greaterp tau 0.0)
		    (setq xValue xValue-tau*(floor (xValue-xStart)/tau )))
		  (setq xVecNew (cons countBasie++ xVecNew))
		  (setq yVecNew (cons xValue yVecNew))))
	      ;; Move j = i+1
	      j++)
	 
	 ;; Create the vector lists.
	 (setq xVecNew (cdr (reverse xVecNew)))
	 (setq yVecNew (cdr (reverse yVecNew)))
	 
					; Create new waveform
	 (setq newWave (drCreateEmptyWaveform))
	 (drPutWaveformXVec newWave (drCreateVec 'double xVecNew))
	 (drPutWaveformYVec newWave (drCreateVec 'double yVecNew))
	 newWave
	 else
	 (artError "crossings: must have > 1 points or start not found.\n")
	 x ))
     ( (famIsFamily x)
       (famMap 'crossings x th direc start tau) )
     ( t
       (artError "crossings: can't handle crossings%L\n"
		 (list x th direc start tau) )))))

/*------------------------------------------------------------------------------
	deriv2: deriv2(x)
	Return the derivative of x at all points of x using the forward-backward 
method. The fn deriv does not do this.

Arg:
x		number/drwave/family
Ret:
		number/drwave/family
*/
(defun deriv2 (x)
  (let  (xVec yVec l yVecNew j newWave)
    (cond 
      ( (numberp x)
        x
      )
      ( (drIsWaveform x)
        ; Get vectors.
        xVec=(drGetWaveformXVec x)
        yVec=(drGetWaveformYVec x)

        ; Get length of vectors.
        l=(drVectorLength xVec)

        ; Create new y vector.
        yVecNew=(drCreateVec 'double l)

        ; Set 1st point.
        (drSetElem yVecNew 0
          ((drGetElem yVec 1)-(drGetElem yVec 0)) /
          ((drGetElem xVec 1)-(drGetElem xVec 0))
        )

        ; Scan middle part of waveform.
        j=2
        (for i 0 l-3
          (drSetElem yVecNew i+1
            ((drGetElem yVec j)-(drGetElem yVec i)) /
            ((drGetElem xVec j)-(drGetElem xVec i))
          )
          j++
        )

        ; Set last point.
        (drSetElem yVecNew l-1
          ((drGetElem yVec l-2)-(drGetElem yVec l-1)) /
          ((drGetElem xVec l-2)-(drGetElem xVec l-1))
        )

        ; Create new waveform
        newWave=drCreateEmptyWaveform()
        (drPutWaveformXVec newWave xVec)
        (drPutWaveformYVec newWave yVecNew)
        newWave
      )
      ( (famIsFamily x)
        (famMap 'deriv2 x)
      )
      ( t
        (artError "deriv2: can't handle deriv2%L\n" (list x)) ))))

/*------------------------------------------------------------------------------
	dftPS: dftPS(x start stop n)
	dft has the same problem as value. See valuePS.

Arg:
x		number/drwave/family
start		number/drwave/family
stop		number/drwave/family
n		number/drwave/family
Ret:
		number/drwave/family
*/
(defun dftPS (x start stop n)
  (let  (yVecStart yVecStop)
    (cond 
      ( (numberp x)
        x )
      
      ( (drIsWaveform x)
        (cond 
          ( (numberp start) && (numberp stop)
            ; Apply function.
            dft(x start stop n)
          )
          ; x is a drWaveID, so its a family of curves.
          ( (drIsWaveform start) && (drIsWaveform stop)
            yVecStart=(drGetWaveformYVec start)
            yVecStop=(drGetWaveformYVec stop)
            ;(when  --daisyCSFCountDftPS<0
            ;  daisyCSFCountDftPS=(drVectorLength yVecStart)-1
            ;)
            ; Apply function.
            dft(x (drGetElem yVecStart daisyCSFCountDftPS)
              (drGetElem yVecStop daisyCSFCountDftPS++) n
            )
          )
          ( (drIsWaveform start) && (numberp stop)
            yVecStart=(drGetWaveformYVec start)
            ;(when  --daisyCSFCountDftPS<0
            ;  daisyCSFCountDftPS=(drVectorLength yVecStart)-1
            ;)
            ; Apply function.
            dft(x (drGetElem yVecStart daisyCSFCountDftPS++) stop n)
          )
          ( (numberp start) && (drIsWaveform stop)
            yVecStop=(drGetWaveformYVec stop)
            ;(when  --daisyCSFCountDftPS<0
            ;  daisyCSFCountDftPS=(drVectorLength yVecStop)-1
            ;)
            ; Apply function.
            dft(x start (drGetElem yVecStop daisyCSFCountDftPS++) n)
          )
          ( t
            (artError "dftPS: can't handle dftPS%L with curve family\n"
              (list x start stop n)
            )
          )
        ) ;cond
      )
      ( (famIsFamily x)
        ; start & stop could be drWaveID, so set the global daisyCSFCountDftPS.
        ; Must scan x in reverse order due to way famMap works!
        daisyCSFCountDftPS=0
        (famMap 'dftPS x start stop n)
      )
      ( t
        (artError "dftPS: can't handle dftPS%L\n" (list x start stop n)) ))))

/*------------------------------------------------------------------------------
	Get wave from family: getFamilyWave(fam n)

	Return the selected wave from a family of waveforms. This allows
functions that operate only upon waveform to work.

Arg:
fam		number/drwave/family
n		number
cycleLen	number
		Length of tail y vector. Vector at bottom of family.
Ret:
		number/drwave
		Returns the n'th wave of the family, where n uses a zero-based
index.
*/
(defun getFamilyWave (fam n @optional (cycleLen 0))
  (let  (xVec yVec (yVecLen 0) (blankIndent "  "))
    (cond 
     ( (numberp fam)
	      fam)
     ( (drIsWaveform fam)
       (if  cycleLen>0 then
		      ;;printf("daisyCSFCountGetFamilyWave=%d\n" daisyCSFCountGetFamilyWave)
		      (if  daisyCSFCountGetFamilyWave++==n then
							(when daisyCSFCountGetFamilyWave>=cycleLen daisyCSFCountGetFamilyWave=0)
							fam
          else
            (when daisyCSFCountGetFamilyWave>=cycleLen daisyCSFCountGetFamilyWave=0)
            nil
          )
        else
          fam
        )
      )
      ( (famIsFamily fam)
        printf("getFamilyWave: Processing family of waveforms of structure;\n")

        ; Get vectors of family.
        xVec=(drGetWaveformXVec fam)
        yVec=(drGetWaveformYVec fam)

        ; Iteratively search tree to determine tail vector length.
        (while  drGetWaveformYType((drGetElem yVec 0))=='waveform
          ; Determine length.
          yVecLen=(drVectorLength (drGetWaveformXVec (drGetElem yVec 0)))

          ; Print info.
          printf("%sLength=%d : xVec type=%L : yVec type=%L\n"
            blankIndent (drVectorLength xVec) drType(xVec) drType(yVec)
          )
          ; Print family sweep parameters.
          (for i 0 (drVectorLength xVec)-1
            printf("%sx(%d)=%g\n" blankIndent i (drGetElem xVec i))
          ) ;for

          ; Move down tree one step.
          yVec=(drGetElem yVec 0)
          xVec=(drGetWaveformXVec yVec)
          yVec=(drGetWaveformYVec yVec)
          ; Move indent in.
          blankIndent=strcat(blankIndent "  ")
        ) ;while

        ; Show tail vector index values.
        printf("%sLength of tail y vector=%d\n" blankIndent yVecLen)
        printf("%sIndex with () values.\n" blankIndent)
        ; Print family sweep parameters.
        (for i 0 (drVectorLength xVec)-1
          printf("%sx(%d)=%g\n" blankIndent i (drGetElem xVec i))
        ) ;for

        ; Get vectors of family.
        xVec=(drGetWaveformXVec fam)
        yVec=(drGetWaveformYVec fam)

        ; Scan family.
        (if  drType(yVec)=='waveform &&
            drGetWaveformYType((drGetElem yVec 0))=='waveform then
          daisyCSFCountGetFamilyWave=0
          (famMap 'getFamilyWave fam n yVecLen)
        else
          (drGetElem yVec n)
        )
      )
      ( t
        (artError "getFamilyWave: can't handle getFamilyWave%L\n" (list fam n)) ))))

/*------------------------------------------------------------------------------
	Greater than function: gt(x th)
Arg:
x		number/drwave/family
th		number/drwave
Ret:
		1.0 if x>th
		0.5 if x==th
		0.0 if x<th
*/
(defun gt (x th)
  (cond 
    ( (numberp x) || (drIsWaveform x)
      (sgn(x-th)+1)*0.5 )

    ( (famIsFamily x)
      (famMap 'gt x th) )
    ( t
      (artError "gt: can't handle gt%L\n" (list x th)) )))

/*------------------------------------------------------------------------------

Changed histogram -> histogramSlice since histogram is a built in function. Christer J. 20080521

	histogramSlice: histogramSlice(x slices)
	Returns a histogram of the x data, sliced into a number of slices.

Arg:
x		number/drwave/family
slices		number
Ret:
		number/drwave/family
*/
(defun histogramSlice (x slices)
  (let  (xMinValue xMaxValue width bin yVec xVecNew yVecNew y newWave)
    (cond 
     ( (numberp x) 
       x )
     ( (drIsWaveform x)
       ;; Create x vector from sigma info.
       slices=fix(slices)
       (if  slices<1 slices=2 )
       (if  slices>1000 slices=1000 )
       xVecNew=(drCreateVec 'double slices+1)
       
       ;; Set x points in center of each slice.
       xMinValue=YMIN(x)
       xMaxValue=YMAX(x)
       width=(xMaxValue-xMinValue) / (slices-1)
       ;; Create new y vector, all set to 0.
       ;; yVecNew=(drCreateVec 'double slices)
       declare( bin[2*slices] )
       (for s 0 slices-1
	    bin[2*s+0]=xMinValue+width*s
	    ;; (drSetElem xVecNew s xMinValue+width*s)
	    bin[2*s+1]=0
	    ;; (drSetElem yVecNew s 0.0)
	    )
       
       ;; Get the y vector.
       yVec=(drGetWaveformYVec x)
       
       ;; Scan waveform and increment each count with a slice <x<=.
       (for i 0 (drVectorLength yVec)-1
	    y=(drGetElem yVec i)
	    ;; find which slice it fits in & count it.
	    (for s 0 slices-1
		 (if  y>=bin[2*s+0]-0.5*width && y<bin[2*s+0]+0.5*width then
		      bin[2*s+1]=bin[2*s+1]+1 )
		 ;;(if  y>=(drGetElem xVecNew s]-0.5*width &&
		 ;;    y<(drGetElem xVecNew s)+0.5*width then
		 ;;  (drSetElem yVecNew s (drGetElem yVecNew s)+1) )
		 ))
       ;; Text output to CIW.
        printf("** HistogramSlice output:\n")
        printf("min/max/width: %g / %g / %g\n" xMinValue xMaxValue width)
        (for s 0 slices-1
          printf("Count(%d)\t[%g]\t= %d\n" s bin[2*s+0] bin[2*s+1])
        )
        printf("Average: %g  Sigma: %g\n" average(x) sigma(x))

        ; Create new vectors.
        xVecNew=(drCreateVec 'double 4*slices)
        yVecNew=(drCreateVec 'double 4*slices)
        ; Assign waveform shape.
        (for s 0 slices-1
          (drSetElem xVecNew 4*s+0 bin[2*s+0]-0.5*width)
          (drSetElem xVecNew 4*s+1 bin[2*s+0]-0.5*width)
          (drSetElem xVecNew 4*s+2 bin[2*s+0]+0.5*width)
          (drSetElem xVecNew 4*s+3 bin[2*s+0]+0.5*width)
          (drSetElem yVecNew 4*s+0 0.0)
          (drSetElem yVecNew 4*s+1 bin[2*s+1])
          (drSetElem yVecNew 4*s+2 bin[2*s+1])
          (drSetElem yVecNew 4*s+3 0.0)
        )
        ; Create new waveform.
        newWave=drCreateEmptyWaveform()
        (drPutWaveformXVec newWave xVecNew)
        (drPutWaveformYVec newWave yVecNew)

        ; Return waveform.
        newWave
      )
      ( (famIsFamily x)
        (famMap 'histogramSlice x slices)
      )
      ( t
        (artError "histogramSlice: can't handle histogramSlice%L\n" (list x slices)) ))))

/*------------------------------------------------------------------------------
	Log to base 2 function: log2(x)
Arg:
x		number/drwave/family
Ret:
		log2(x)
*/
(defun log2 (x)
  (cond 
   ( (numberp x) || (drIsWaveform x)
     ln(x)/ln(2))
   ( (famIsFamily x)
     (famMap 'log2 x))
   ( t
     (artError "log2: can't handle log2%L\n" (list x)) )))

/*------------------------------------------------------------------------------
	Less than function: lt(x th)
Arg:
x		number/drwave/family
th		number/drwave
Ret:
		1.0 if x<th
		0.5 if x==th
		0.0 if x>th
*/
(defun lt (x th)
  (cond 
    ( (numberp x) || (drIsWaveform x)
      (sgn(th-x)+1)*0.5)
    ( (famIsFamily x)
      (famMap 'lt x th))
    ( t
      (artError "lt: can't handle lt%L\n" (list x th)))))

/*------------------------------------------------------------------------------
	mark2space: mark2space(x,th)
	Instantaneous Mark/Space ratio of a wave crossing th. The threshold 
maybe a number or dependent waveform of the same length as x. Such a varying 
threshold allows a moving threshold to be used.

Arg:
x		number/drwave/family
th		number/drwave
Ret:
		number/drwave/family
*/
(defun mark2space (x th)
  (let  (xVec yVec xVecNew yVecNew xValueR xValueR0 xValueF j newWave)
    (cond 
     ( (numberp x) x )
     ( (drIsWaveform x)
       (if  (drVectorLength (drGetWaveformXVec x))>3 then
	    ;; Subtract threshold
	    x=x-th
	    
	    ;; Get vectors.
	    xVec=(drGetWaveformXVec x)
	    yVec=(drGetWaveformYVec x)

          ; Create new vector lists, there start & set reference start.
          xVecNew=(tconc nil 0.0)
          yVecNew=(tconc nil 0.0)
          xValueR0=(drGetElem xVec 0)
          xValueF=(drGetElem xVec 0)

          ; Scan waveform quickly for crossing, than process details.
          j=2
          (for i 1 (drVectorLength (drGetWaveformXVec x))-2

            (when  (drGetElem yVec i)*(drGetElem yVec j)<=0

              ; Rising edge.
              (when  (drGetElem yVec j)-(drGetElem yVec i)>0
                xValueR=xCrossValue(0.0
                  (drGetElem xVec i) (drGetElem yVec i)
                  (drGetElem xVec j) (drGetElem yVec j)
                )

                (when  xValueR>xValueR0
                  (tconc xVecNew xValueR0)
                  (tconc yVecNew (xValueF-xValueR0)/(xValueR-xValueR0) )

                  ; Save last rising edge.
                  xValueR0=xValueR
                ) ;when
              ) ;when right way

              ; Falling edge.
              (when  (drGetElem yVec j)-(drGetElem yVec i)<0
                xValueF=xCrossValue(0.0
                  (drGetElem xVec i) (drGetElem yVec i)
                  (drGetElem xVec j) (drGetElem yVec j)
                )
              ) ;when right way

            ) ;when crossed

            ; Move j = i+1
            j++

          ) ;for

          ; Create the vector lists.
          xVecNew=car(xVecNew)
          yVecNew=car(yVecNew)

          ; Set 2nd y vec to 3rd value and 1st y vec to 2nd value if possible.
          ; This stops discontinuity at start.
          yVecNew=(drCreateVec 'double yVecNew)
          (when  (drVectorLength yVecNew)>3
            (drSetElem yVecNew 1 (drGetElem yVecNew 2))
          )
          (when  (drVectorLength yVecNew)>2
            (drSetElem yVecNew 0 (drGetElem yVecNew 1))
          )

          ; Create new waveform
          newWave=drCreateEmptyWaveform()
          (drPutWaveformXVec newWave (drCreateVec 'double xVecNew))
          (drPutWaveformYVec newWave yVecNew)
          newWave
        else
          (artError "mark2space: must have > 3 points.\n")
          x
        )
      )
      ( (famIsFamily x)
        (famMap 'mark2space x th)
      )
      ( t
        (artError "mark2space: can't handle mark2space%L\n" (list x th)) ))))

/*------------------------------------------------------------------------------
	Max X value: maxX(x)
Arg:
x		number/drwave/family
Ret:
		number/drwave
*/
(defun maxX (x)
  (cond 
   ( (numberp x)
     x )
   ( (drIsWaveform x)
     (drGetElem (drGetWaveformXVec x) (drVectorLength (drGetWaveformXVec x))-1))
   ( (famIsFamily x)
     (famMap 'maxX x))
   ( t
     (artError "maxX: can't handle maxX%L\n" (list x)))))

/*------------------------------------------------------------------------------
	Min X value: minX(x)
Arg:
x		number/drwave/family
Ret:
		number/drwave
*/
(defun minX (x)
  (cond 
    ( (numberp x) x)
    ( (drIsWaveform x)
      (drGetElem (drGetWaveformXVec x) 0))
    ( (famIsFamily x)
      (famMap 'minX x))
    ( t
      (artError "minX: can't handle minX%L\n" (list x)))))

/*------------------------------------------------------------------------------
	overlayPeriod: overlayPeriod(x start tau)
	Return a family of waveforms of x split up onto consecutive periods of 
tau after start.

\i VT("/net91")
\t (family \0\.6 drwave:50163752 \0\.8 drwave:50163744
\t     \1 drwave:50163736 sweepVariable "Vin" sweepVariableType
\t     float
\t )

Arg:
x		number/drwave/family
start		number
tau		number
Ret:
		number/drwave/family
*/
(defun overlayPeriod (x start tau)
  (let  (xVec yVec vecLen xVecNew yVecNew yFlyback newWave i)
    
    (cond 
     ( (numberp x) x)
     ( (drIsWaveform x)
       ;; Get vectors.
       xVec=(drGetWaveformXVec x)
       yVec=(drGetWaveformYVec x)

        ; Check waveform length.
        vecLen=(drVectorLength xVec)
        (if  vecLen>2 then

          ; Check start is after 1st point of wave form.
          (if  start>=(drGetElem xVec 0) then
            ; Scan to start time.
            i=0
            (while  (drGetElem xVec i)<start
              i++
            )

            ; Start new waveform vector list's.
            xVecNew=(tconc nil 0.0)
            yVecNew=(tconc nil
              yCrossValue(start
                (drGetElem xVec i) (drGetElem yVec i)
                (drGetElem xVec i+1) (drGetElem yVec i+1)
              )
            )

            ; Scan to end of wave.
            ; Until period has gone beyond end of wave.
            ;yFlyback=1.1*YMIN(x)-0.1*YMAX(x)
            ; Magic value stop trace display!!!
            yFlyback=5e37
            (while  i<vecLen-1
              ; Check end of period not reached.
              (when  (drGetElem xVec i)-start>=tau
                ; Append fly-back trace.
                (tconc xVecNew 1.1*tau)
                (tconc yVecNew yFlyback)
                (tconc xVecNew -0.1*tau)
                (tconc yVecNew yFlyback)
                ; Advance start.
                start=start+tau
              )

              ; Append data until period over.
              (tconc xVecNew (drGetElem xVec i)-start)
              (tconc yVecNew (drGetElem yVec i))

              ; Move along.
              i++
            ) ;while

            ; Select new wave vectors.
            xVecNew=(drCreateVec drGetWaveformXType(x) car(xVecNew))
            yVecNew=(drCreateVec drGetWaveformYType(x) car(yVecNew))

            ; Create new waveform.
            newWave=drCreateEmptyWaveform()
            (drPutWaveformXVec newWave xVecNew)
            (drPutWaveformYVec newWave yVecNew)

            ; Return waveform.
            newWave
          else
            (artError "overlayPeriod: start<%g\n" (drGetElem xVec 0))
            x
          )
        else
          (artError "overlayPeriod: pts<3\n")
          x
        )
      )

      ( (famIsFamily x)
        (famMap 'overlayPeriod x start tau)
      )

      ( t
        (artError "overlayPeriod: can't handle overlayPeriod%L\n"
          (list x start tau) )))))

/*------------------------------------------------------------------------------
	pkDet: pkDet(x)
	Performs a peak detection function upon x.

Arg:
x		number/drwave/family
Ret:
		number/drwave/family
*/
(defun pkDet (x)
  (let  (xVec yVec yMax xVecNew yVecNew newWave j xX)
    (cond 
     ( (numberp x) x )
     ( (drIsWaveform x)
       ;; Get vectors.
       xVec=(drGetWaveformXVec x)
       yVec=(drGetWaveformYVec x)
       
       ;; Start new waveform vector list's.
       xVecNew=(tconc nil (drGetElem xVec 0))
       yMax=(drGetElem yVec 0)
       yVecNew=(tconc nil yMax)

        ; Scan the whole waveform. i is the leading point.
        (for i 1 (drVectorLength yVec)-1

          ; Check if y value exceeds maximum y so far.
          (when  (drGetElem yVec i)>yMax
            j=i-1
            ; It does so add new point to new vector and update maximum y.
            ; First find point where y > yMax.
            xX=xCrossValue(yMax
              (drGetElem xVec j) (drGetElem yVec j)
              (drGetElem xVec i) (drGetElem yVec i)
            )

            ; Add data point at crossing point if xX less than sample pt.
            (when  xX>(drGetElem xVec j)
              (tconc xVecNew xX)
              (tconc yVecNew yMax)
            )

            ; Note next point along curve.
            yMax=(drGetElem yVec i)
            (tconc xVecNew (drGetElem xVec i))
            (tconc yVecNew yMax)
          ) ;when

        ) ;for

        ; Select new wave vectors.
        xVecNew=(drCreateVec drGetWaveformXType(x) cdar(xVecNew))
        yVecNew=(drCreateVec drGetWaveformYType(x) cdar(yVecNew))

        ; Create new wave.
        newWave=drCreateEmptyWaveform()
        (drPutWaveformXVec newWave xVecNew)
        (drPutWaveformYVec newWave yVecNew)

        ; Return waveform.
        newWave
      )
      ( (famIsFamily x)
        (famMap 'pkDet x)
      )
      ( t
        (artError "pkDet: can't handle pkDet%L\n" (list x)) ))))

(defun pkDet2 (x)
  (let  (xVec yVec j newWave)
    (cond 
     ( (numberp x) x)
     ( (drIsWaveform x)
       ;; Get vectors.
       xVec=(drGetWaveformXVec x)
       yVec=(drGetWaveformYVec x)
       ;; Scan the whole waveform. i is the leading point.
       j=0
       (for i 1 (drVectorLength yVec)-1
	    ;; Check if y value is less than previous y value.
	    (when  (drGetElem yVec i)<(drGetElem yVec j)
		  (drSetElem yVec i (drGetElem yVec j))
          )
          j++
        ) ;for

        ; Create new wave.
        newWave=drCreateEmptyWaveform()
        (drPutWaveformXVec newWave xVec)
        (drPutWaveformYVec newWave yVec)

        ; Return waveform.
        newWave
      )
      ( (famIsFamily x)
        (famMap 'pkDet2 x)
      )
      ( t
        (artError "pkDet2: can't handle pkDet2%L\n" (list x))
      )
    )
  ) ;let
) ;proc


/*------------------------------------------------------------------------------
	runDelay: runDelay(a b [[[th] direc] start])
	Returns the delay of waveform 'b' to waveform 'a'. The delay is determined by either waveform crossing the threshold 'th' in the direction 'direc'. Both 'a' and 'b' are scanned from time 'start'. The returned independent axis is the crossing times of waveform 'a'.

Arg:
a		number/drwave/family
b		number/drwave/family

th		number/drwave	0.0
		Crossing threshold.

direc		string
		Crossing direction.

start		number
		Start time.
Ret:
		number/drwave/family
*/
(defun runDelay (a b @optional (th 0.0) (direc "either") (start 0.0))
  (let  (ca cb xVecNew yVecNew yVecA yVecB newWave)
    (cond 
      ( (numberp a) a)
      ( (drIsWaveform a) && (drIsWaveform b)
        (if  (drVectorLength (drGetWaveformXVec a))>1 then
	     ;; Get the threshold crossing times of a & b.
	     ca=crossings(a th direc start)
	     cb=crossings(b th direc start)
	     
	     ;; Get the shortest result.
	     (if  (drVectorLength (drGetWaveformXVec ca))<
		  (drVectorLength (drGetWaveformXVec cb)) then
		  xVecNew=(drGetWaveformXVec ca)
		  yVecNew=(drGetWaveformYVec ca)
		  else
		  xVecNew=(drGetWaveformXVec cb)
            yVecNew=(drGetWaveformYVec cb)
          )
          yVecA=(drGetWaveformYVec ca)
          yVecB=(drGetWaveformYVec cb)

          ; Compute the delay.
          (for i 0 (drVectorLength xVecNew)-1
            ; Set xVecNew to time of a crossing.
            (drSetElem xVecNew i (drGetElem yVecA i))
            ; Set yVecNew to delay from a crossing to b crossing.
            (drSetElem yVecNew i
              (drGetElem yVecB i)-(drGetElem yVecA i)
            )
          ) ;for

          ; Create new waveform
          newWave=drCreateEmptyWaveform()
          (drPutWaveformXVec newWave xVecNew)
          (drPutWaveformYVec newWave yVecNew)
          newWave
        else
          (artError "runDelay: must have > 1 points.\n")
          x
        )
      )
      ( (famIsFamily a)
        (famMap 'runDelay a b th direc start)
      )
      ( t
        (artError "runDelay: can't handle runDelay%L\n" (list a b th direc start))
      )
    )
  ) ;let
) ;proc


/*------------------------------------------------------------------------------
	samplePS: samplePS(x start stop byType by)
	Parametric sweep version of sample.

Arg:
		as sample.
Ret:
		as sample.
*/
(defun samplePS (x start stop byType by)  
  (cond 
   ( (famIsFamily x)
     (famMap 'sample x start stop byType by))
   ( t
     (sample x start stop byType by))))

/*------------------------------------------------------------------------------
scaleX: scaleX(x n)
	Scale the X axis by the required factor. Multiplies each value of the 
independent x axis by the factor n.

Arg:
x		number/drwave/family
n		number
		Scale factor.
Ret:
		number/drwave/family
		Reduce data set of x
*/
(defun scaleX (x n)
  (let  (xVec xVecNew newWave)   
    (cond 
     ( (numberp x)
       x*n )
     ( (drIsWaveform x)
       ;; Get copy of vectors.
       xVec=(drGetWaveformXVec x)
       xVecNew=(drCreateVec drType(xVec) (drVectorLength xVec))

       ;; Scan waveform and scale each value of x.
       (for i 0 (drVectorLength xVec)-1
	    (drSetElem xVecNew i (drGetElem xVec i)*n))

       ;; Create new waveform.
       newWave=drCreateEmptyWaveform()
       (drPutWaveformXVec newWave xVecNew)
       (drPutWaveformYVec newWave (drGetWaveformYVec x))
       
       ;; Return waveform.
        newWave )
     ( (famIsFamily x)
       (famMap 'scaleX x n))
     ( t
       (artError "scaleX: can't handle scaleX%L\n" (list x n)) ))))

/*------------------------------------------------------------------------------
	Sigum function: sgn(x)
Arg:
x		number/drwave/family
Ret:
		0.0 if x==0
		1.0 if x<0
		-1.0 if x<0
*/
(defun sgn (x)
  (cond 
   ( (numberp x) || (drIsWaveform x)
     (if  x==0.0
	 x-x
       x/abs(x) ))
   ( (famIsFamily x)
     (famMap 'sgn x))
   ( t
     (artError "sgn: can't handle sgn%L\n" (list x)) )))


/*------------------------------------------------------------------------------
	Standard Deviation Sigma: sigma(x)
Arg:
x		number/drwave/family
Ret:
		number/drwave
		sigma(x)=sum (x-<x>)/n
*/
(defun sigma (x)
  (cond 
   ( (numberp x) x)
   ( (drIsWaveform x)
     (rms x-(average x)))
   ( (famIsFamily x)
     (famMap 'sigma x))
   ( t
     (artError "sigma: can't handle sigma%L\n" (list x)))))

/*------------------------------------------------------------------------------
	skipPts: skipPts(x n)
	Skips data points of a wave, i.e. it misses simulator time pts. Takes 
the n'th data point of x.

Arg:
x		number/drwave/family
n		number
		Specifies the n'th pt to use.
Ret:
		number/drwave/family
		Reduce data set of x
*/
(defun skipPts (x n)
  (let  (xVec yVec xVecNew yVecNew newWave)

    (cond 
      ( (numberp x) x)
      ( (drIsWaveform x)
	;; Get vectors.
        xVec=(drGetWaveformXVec x)
        yVec=(drGetWaveformYVec x)
	
	;; Start new waveform vector list's.
        xVecNew=(tconc nil (drGetElem xVec 0))
        yVecNew=(tconc nil (drGetElem yVec 0))
	
	;; Ensure n is an integer.
        (setq n (round n))
	
	;; Scan waveform for every n'th pt.
        (for i 0 fix(((drVectorLength xVec)-1)/n)
	     ;; Add next n'th pt.
	     (tconc xVecNew (drGetElem xVec i*n))
	     (tconc yVecNew (drGetElem yVec i*n)))

	;; Build the new vectors.
        xVecNew=(drCreateVec drGetWaveformXType(x) cdar(xVecNew))
        yVecNew=(drCreateVec drGetWaveformYType(x) cdar(yVecNew))

        ; Create new waveform.
        newWave=drCreateEmptyWaveform()
        (drPutWaveformXVec newWave xVecNew)
        (drPutWaveformYVec newWave yVecNew)
        
        ; Return waveform.
        newWave
      )
      ( (famIsFamily x)
        (famMap 'skipPts x n)
      )
      ( t
        (artError "skipPts: can't handle skipPts%L\n" (list x n))
      )
    ) ;cond

  ) ;let
) ;proc


/*------------------------------------------------------------------------------
	time2freq: time2freq(x [[[th] direc] start])
	Instantaneous freq of a wave crossing 'th'. The threshold maybe a number or dependent waveform of the same length as x. Such a varying threshold allows a moving threshold to be used. The crossing direction is indicated by 'direc'. The waveform is scanned from 'start' onwards.

Arg:
x		number/drwave/family
th		number/drwave
		Optional after x.
direc		string
		Optional after th.
start		number
		Optional after direc.
Ret:
		number/drwave/family
*/
(defun time2freq (x @optional (th 0.0) (direc "either") (start 0.0))
  (let  (r (tauMod 1.0) xVecNew yVec yVecNew newWave)
    (cond 
      ( (numberp x) x)
      ( (drIsWaveform x)
        (if  (drVectorLength (drGetWaveformXVec x))>3 then
	     ;; Get crossing times.
	     r=crossings(x th direc start)
	     
	     ;; Get vectors.
	     xVecNew=(drGetWaveformXVec r)
	     yVec=(drGetWaveformYVec r)
	     yVecNew=(drCreateVec drType(yVec) (drVectorLength yVec))
	     
	     ;; Set tau of period.
	     (when direc=="either" tauMod=0.5)
	     
	     ;; Scan waveform quickly for crossing, than process details.
	     (for i 1 (drVectorLength xVecNew)-1
		  ;; Set x to crossing time.
		  (drSetElem xVecNew i (drGetElem yVec i))
					;; Set y to 1/(xn - xn-1)
		  (drSetElem yVecNew i tauMod/((drGetElem yVec i)-(drGetElem yVec i-1))))
	     ;; Set 1st value equal to 2nd value.
	     ;; This stops discontinuity at start.
	     (drSetElem xVecNew 0 (drGetElem yVecNew 0))
	     (drSetElem yVecNew 0 (drGetElem yVecNew 1))
	     
	     ;; Create new waveform
	     newWave=drCreateEmptyWaveform()
	     (drPutWaveformXVec newWave xVecNew)
	     (drPutWaveformYVec newWave yVecNew)
	     newWave
	     else
	     (artError "time2freq: must have > 3 points.\n") )
	x)
      ( (famIsFamily x)
	(famMap 'time2freq x th direc start) )
      
      ( t
	(artError "time2freq: can't handle time2freq%L\n" (list x th direc start)) ))))

/*------------------------------------------------------------------------------
	Two complement twoscomp(x n)
Arg:
x		number/drwave/family
		Binary number as decimal.
n		number
		Number of binary bits in word.
Ret:
		number
		Decimal equivalent of binary twos complement.
*/
(defun twoscomp (x n)
  x -  (2**n)*(gt x 2**(n-1)-0.5))



/*------------------------------------------------------------------------------
	Value of y at x: valuePS(y x)
	There are problem with value, when given families of curves. Expressions 
of the form;
        value( expression expression_and_not_a_number_as_usual )
present
	value( fam_drWaveID_list drWaveID )
to value. As value expects arg 2 to be a number this gives a problem.
	The following code solves this.

Arg:
y		number/drwave/family
x		number/drwave
Ret:
		number/drwave/family
*/
(defun valuePS (y x)
  (let  (yVec)
    (cond 
     ( (numberp y) y)
     ( (drIsWaveform y)
       (cond 
	( (numberp x)					; Apply function.
	  (value y x))
	( (drIsWaveform x) 					; x is a drWaveID, so its a family of curves.
	  yVec=(drGetWaveformYVec x) 					; Apply function.
	  (value y (drGetElem yVec daisyCSFCountValuePS++)))
	( t
	  (artError "valuePS: can't handle valuePS%L with curve family\n"
		    (list y x)))))
    ( (famIsFamily y)
					; x could be a drWaveID, so set the global daisyCSFCountValuePS.
      daisyCSFCountValuePS=0
      (famMap 'valuePS y x))
    ( t
      (artError "valuePS: can't handle valuePS%L\n" (list y x))))))

/*------------------------------------------------------------------------------
	Independent vector of a waveform: xVal(x)
Arg:
x		number/drwave/family
Ret:
		number/drwave/family
		Independent x variable.
*/
(defun xVal (x)
  (let  (newWaveform xVec)
    (cond 
     ( (numberp x) x)
     ( (drIsWaveform x)
       newWaveform=drCreateEmptyWaveform()
       xVec=(drGetWaveformXVec x)
       (drPutWaveformXVec newWaveform xVec)
       (drPutWaveformYVec newWaveform xVec)
       newWaveform )
     ( (famIsFamily x)
       (famMap 'xVal x) )
     ( t
       (artError "xVal: can't handle xVal%L\n" (list x)) ))))

/*------------------------------------------------------------------------------
	NO LONGER USED.

;;;;;;;;;;;;;;;;;;;;;
; eyeplot_4.3.4.il  -  File "eyeplot.il" modified to run under 4.3.4
;;;;;;;;;;;;;;;;;;;;;  (for Mark Nadim--8/95).
(defun eyeplot ( waveform period )
  (let  ( windowId xvec len x x1 x2 famWaveform newWaveform )
    windowId = awvGetCurrentWindow()
    xvec     = (drGetWaveformXVec  waveform )
    len      = (drVectorLength  xvec )
    x1       = (drGetElem  xvec 0     )
    x2       = (drGetElem  xvec len-1 )
    x = x1
    famWaveform = (famCreateFamily "T" 'float )  ; 'double for 4.4; 'float 4.34
    (while  x  <=  x2 - 0.9*period
	    newWaveform = clip(waveform x x+period)
	    newWaveform = lshift( newWaveform x )
	    famAddValue( famWaveform concat(sprintf(nil "%L" x)) newWaveform )
	    x = x + period)
    famWaveform ))

*/


;===============================================================================
; Data entry.

/*------------------------------------------------------------------------------
	table: table(x ...)
TABLE(x,x0,y0,x1,y1,... xn,yn)

Returns the linearly interpolated y value that corresponds to argument x, based 
on the piecewise-linear table of (xi,yi) pairs. If x<=x0, y0 is returned and if 
y>=xn, yn is returned. Any number of (x,y) pairs can be passed. Scale the X axis 
by the required factor. Multiplies each value of the independent x axis by the 
factor n.
	Calls wave().

Arg:
x		number
...		pairs of numbers in form x0, y0, ..., xn, yn

Ret:
		number
		Value of y at x.
*/
(defun table (x @rest args)
  ; Return answer.
  (value  (apply 'wave args) x))

/*------------------------------------------------------------------------------
	wave: wave(x ...)
	Return a drWave using the x-y data pairs passed to it.

Arg:
...		pairs of numbers in form x0, y0, ..., xn, yn

Ret:
		drwave
		Waveform with y against x.
*/
(defun wave  (@rest args)
  (let  ((i t) xVecNew yVecNew newWave)
    
    ;; Check for x-y pairs.
    (if  mod(length(args) 2)==0 then
	 ;; Create new vector lists, there start & set reference start.
	 xVecNew=(tconc nil nil)
	 yVecNew=(tconc nil nil)
	 
	 ;; Build wave.
	 (foreach  item args
		   (if  i then
		     (tconc xVecNew item)
		     else
		     (tconc yVecNew item))
		   i=!i )
	 ;; Create the vector lists.
	 xVecNew=(drCreateVec 'double cdar(xVecNew))
	 yVecNew=(drCreateVec 'double cdar(yVecNew))
	 
	 ;; Create new wave.
	 newWave=drCreateEmptyWaveform()
	 (drPutWaveformXVec newWave xVecNew)
	 (drPutWaveformYVec newWave yVecNew)
	 
	 ;; Return the wave.
	 newWave
	 else
	 (artError "wave/table: xn,yn points must be in pairs for wave%L\n"
		   (list args))
	 ;; Return 0.
	 0.0)))
					;===============================================================================
					; Data storage.

/*------------------------------------------------------------------------------
dumpWave2file: dumpWave2file(x file)
	Dumps a wave to a file in full data resolution of 15 decimal places.

Args:
x		number/drwave/family
		Wave data to dump.
file		string
		File to dump to.
Ret:
x
*/
(defun dumpWave2file (x file)
  (let  (xVec yVec fp)
    (cond 
     ( (numberp x) x )
     ( (drIsWaveform x)
       ;; Get vectors.
       xVec=(drGetWaveformXVec x)
       yVec=(drGetWaveformYVec x)
       
       ;; Open file.
       fp=outfile(file)
       ;;(fprintf fp "xVal yVal\n")
       
       ;; Dump wav
       (for i 0 (drVectorLength xVec)-1
	    (fprintf fp "%-.15e\t%-.15e\n"
		     (drGetElem xVec i) 
		     (drGetElem yVec i) ))
       
       ;; Close file
       (close fp)
       
       ;; Return wave. And why not.
       x)
     ( (famIsFamily x)
       (famMap 'dumpWave2file x file))
     ( t
       (artError "dumpWave2file: can't handle dumpWave2file%L\n" (list x file)) ))))

;;===============================================================================
					; Impedance measurment.

/*------------------------------------------------------------------------------
Define functions for measuring input impedance.
The operations are as follows;
 For AC analysis only.
  RinS - Equivalent R of a series R + jX impedance.
  XinS - Equivalent X of a series R + jX impedance.
  CinS - Equivalent C of a series R + jX impedance.
  LinS - Equivalent L of a series R + jX impedance.
  RinP - Equivalent R of a parallel R // jX impedance.
  XinP - Equivalent X of a parallel R // jX impedance.
  CinP - Equivalent C of a parallel R // jX impedance.
  LinP - Equivalent L of a parallel R // jX impedance.
All functions require;
  Buffer = Current terminal 'if'
  1st on Stack = Voltage node 'vf'
Thus to use, enter
 1 - the voltage across the port, looking into the port.
 2 - the current port, with +ve current flowing into the port.
Arg:
I		number/drwave/family
V		number/drwave/family
Ret:
		number/drwave/family
		As above.
*/

; Rin Series.
(defun RinS (I V)
  (cond 
    ( (drIsWaveform I) && (drIsWaveform V)
      real(V/I) )
    ( (famIsFamily I) && (famIsFamily V)
      (famMap 'RinS I V))
    ( t
      (artError "RinS: can't handle RinS%L\n" (list I V)) )))
; Xin Series.
(defun XinS (I V)
  (cond 
    ( (drIsWaveform I) && (drIsWaveform V)
      i(mag V/I) )
    ( (famIsFamily I) && (famIsFamily V)
      (famMap 'XinS I V) )
    ( t
      (artError "XinS: can't handle XinS%L\n" (list I V)) )))

; Cin Series.
(defun CinS (I V)
  (cond 
   ( (drIsWaveform I) && (drIsWaveform V)
     -1.0/(6.28318530717958647688*XinS(I V)*xVal(I)) )
   ( (famIsFamily I) && (famIsFamily V)
     (famMap 'CinS I V))
   ( t
     (artError "CinS: can't handle CinS%L\n" (list I V)))))

;; Lin Series.
(defun LinS (I V)
  (cond 
   ( (drIsWaveform I) && (drIsWaveform V)
     XinS(I V)/(6.28318530717958647688*xVal(I)))
   ( (famIsFamily I) && (famIsFamily V)
     (famMap 'LinS I V))
   ( t
     (artError "LinS: can't handle LinS%L\n" (list I V)) )))

;; Rin Parallel.
(defun RinP (I V)
  (cond 
   ( (drIsWaveform I) && (drIsWaveform V)
     (mag V)/((mag I)*cos((phase(I)-phase(V))/57.2957795130823208772)) )
   ( (famIsFamily I) && (famIsFamily V)
     (famMap 'RinP I V))
   ( t
     (artError "RinP: can't handle RinP%L\n" (list I V)) )))

;; Xin Parallel.
(defun XinP (I V)
  (cond 
   ( (drIsWaveform I) && (drIsWaveform V)
     (mag V)/((mag I)*sin((phase(I)-phase(V))/57.2957795130823208772)) )
   ( (famIsFamily I) && (famIsFamily V)
     (famMap 'XinP I V) )
   ( t
     (artError "XinP: can't handle XinP%L\n" (list I V)) )))

;; Cin Parallel.
(defun CinP (I V)
  (cond
     ( (and (drIsWaveform I) (drIsWaveform V))
       1.0/(6.28318530717958647688*(XinP I V)*(xVal I)) )
     ( (and (famIsFamily I) (famIsFamily V))
       (famMap 'CinP I V))
     ( t
       (artError "CinP: can't handle CinP%L\n" (list I V)) )))

; Lin Parallel.
(defun LinP (I V)
  (cond 
    ( (drIsWaveform I) && (drIsWaveform V)
      -XinP(I V)/(6.28318530717958647688*xVal(I)))
    ( (famIsFamily I) && (famIsFamily V)
      (famMap 'LinP I V) )
    ( t
      (artError "LinP: can't handle LinP%L\n" (list I V)))))

;; ------------------------------------------------------------------------------
;;	Port impedance expressed as acVSWR.
;; Arg:
;; I		number/drwave/family
;; V		number/drwave/family
;; Zo		number
;; 		Reference impedance driving the port.
;; Ret:
;; 		number/drwave/family
;; 		As above.
;;

(defun acVSWR (I V Zo)
  (let (Z rho)    
    (cond
     ( (drIsWaveform I) && (drIsWaveform V)
       Z=V/I
       rho=abs( (Zo-Z)/(Zo+Z) )
       abs( (1+rho)/(1-rho) ) )
     ( (famIsFamily I) && (famIsFamily V)
       (famMap 'acVSWR I V Zo) )
     ( t
       (artError "acVSWR: can't handle acVSWR%L\n" (list I V Zo)) ))))

/*------------------------------------------------------------------------------
	A selection of function for evaluating port impedances from 
s-parameters.

Arg:
P		number
		Port number, e.g., 1, 2.
Zo		number.
		Impedance of port source/load.
Ret:
		number/drwave/family
*/
(defun Zport (P Zo)
  Zo * (1+aaSP(P,P)) / (1-aaSP(P,P)) )

(defun Yport (P Zo)
  1.0/Zport(P Zo))

;===============================================================================
; Noise.
; Done by entry procedures.


;-------------------------------------------------------------------------------
; Reset writeProtect status.

(sstatus writeProtect daisyWriteProtectStatus)

(defun CreateMyForm ()			  ;_Jun 10 04 wikner 267
  (let  ( fieldList a b )
    (setq a (ahiCreateStringField 
	     ?name 'from
	     ?prompt "From"
	     ?value ""))
    (setq b (ahiCreateStringField 
	     ?name 'to
	     ?prompt "To"
	     ?value ""))
    (setq fieldList (list 
		     (list a 5:0 120:25 40 )
		     (list b 160:0 110:25 30 )))
    (calCreateSpecialFunctionsForm 'MyForm 
				   fieldList )))

(defun MySpecialFunctionCB ()		  ;_May 12 04 wikner 0
  (calCreateSpecialFunction
   ?formSym 'MyForm
   ?formInitProc 'CreateMyForm
   ?formTitle "Test"
   ?formCallback "calSpecialFunctionInput( 'test '(from to) )" ))

(defun trTopLevelSubst ( aList aLookUpList )	  ;_Jun 10 04 wikner 0
  (let ( currentElement substValue )
    (foreach map listCell aList
	     (setq currentElement (car  listCell ))
	     (setq substValue (cadr (assoc currentElement aLookUpList )))
	     (when substValue
	       (rplaca listCell substValue )) )
    aList ))

(defun therm2dec (@rest bits)  
  (let   (xVecNew yVecNew n newWave)     
    ;; Start result vectors.
    (setq xVecNew (drGetWaveformXVec (car bits)))
    (setq yVecNew (drCreateVec 'intlong (drVectorLength xVecNew)))    
    (for i 0 (drVectorLength xVecNew)-1
	 ;; Reset count to 0.
	 (setq n 0)
	 (for b 0 (length bits)-1
	      ;; printf("i=%d b=%d n=%g\n" i b n)
	      (when (abs (drGetElem (drGetWaveformYVec (nth b bits)) i)) >= 0.5
		    (setq n n+1) ))
	 ;; Set decimal value of bits.
	 (drSetElem yVecNew i n))
    ;; Create new waveform.
    (setq newWave drCreateEmptyWaveform)
    (drPutWaveformXVec newWave xVecNew)
    (drPutWaveformYVec newWave yVecNew)    
    ;; Return waveform.
    newWave     ))

(defun PresentRegionsCB ()			  ;_May 12 04 wikner 871
  (let (selectedTransistors 
	operationRegions 
	operationRegion 
	selectedObjects 
	listOfTransistors)       
    
    ;;(setq currentSession  (asiGetSession "spectre0")) ; Assume simulation finished 
    ;;(setq currentTopCellView (asiGetTopCellView currentSession)) ; Notice that this should be changed
    ;;    currentTopCellView = geGetWindowCellView()
    ;;    geDeselectAllFig(currentTopCellView)
    ;;    geSelectAllFig(currentTopCellView)
    (setq selectedObjects (geGetSelSet)) ;; currentTopCellView)
    ;;    geDeselectAllFig(currentTopCellView)
    (setq listOfTransistors (list "pch_25" "nch_25" "nch" "nch_33" "nch_33_dnw" "nch_33_dnwx" "nch_33x" "nch_dnw" "nch_dnwx" "nch_hvt" "nch_hvt_dnw" "nch_hvt_dnwx" "nch_hvtx" "nch_lvt" "nch_lvt_dnw" "nch_lvt_dnwx" "nch_lvtx" "nch_na" "nch_na33" "nch_na33x" "nch_nax" "nchx" "pch" "pch_33" "pch_33x" "pch_hvt" "pch_hvtx" "pch_lvt" "pch_lvtx" "pchx" ))
    ;;schHiFind("cellName" "!=" "c_ntrans")x
    ;;schSchFindForm->allNone->none->value =  t
    ;;schSchFindForm->objects->instance->value =  t
    ;;schSchFindForm->instAllNone->none->value =  t
    ;;hiFormApply(schSchFindForm)
    
    (setq RegionLookUp (list 
			(list "0" "Cut-off" )
			(list "1" "Linear" )
			(list "2" "Saturation" )
			(list "3" "Subthreshold") 
			(list "4" "Break-down")))
    ;; Primitive but effective selection of transistors
    
    (printf "\n=====================\n")
    (printf "List of transistors and their operation regions in %s.\n" currentTopCellView~>cellName)
    (printf "=====================\n")
    
    (foreach singleObject selectedObjects 
	     (when member(singleObject ~> cellName listOfTransistors)
		   (setq mathExp (strcat "OP(\"/" singleObject~>name "\",\"region\") "))
	  calCalcInput( 'clear )
	  _ahiiUniqueSymbolformcalculator0->buffer->value = mathExp
	  ;;; caliPrintCB()				
	  ;;; Here we should check if it is a parametric sweep
	  ;;; or not
	  _ahiiUniqueSymbolformcalculator0->eval->value =  t
	  _ahiiUniqueSymbolformcalculator0->eval->value =  nil
	  (setq operationRegion _ahiiUniqueSymbolformcalculator0 -> buffer -> value)
	  (setq selectedTransistors (cons singleObject~>name selectedTransistors ))
	  (setq operationRegionString (trTopLevelSubst (list operationRegion) RegionLookUp))
	  (printf "Transistor %s : %s region \n" singleObject~>name (car operationRegionString)) ))
    
    (calCalcInput 'clear )
    _ahiiUniqueSymbolformcalculator0->buffer->value = "Please look in the CIW!"
    (printf "=====================\n") ))

(calRegisterSpecialFunction
 (list  "test" 'MySpecialFunctionCB )) 

(calRegisterSpecialFunction
 (list "*DC operation regions" 'PresentRegionsCB ))   

(defun asiPresentRegions ()			  ;_Jun 24 04 wikner 1854
  ;; Used for short-cut key.
  (let (( selectedTransistors (list ))
	( operationRegion  "" )
	( selectedObjects  (list ) )
	( regionLookUp (list "Cut-off" "Linear" "Saturation" "Subthreshold" "Break-down"))
	
	( listOfTransistors (list "nch_25" "pch_25" "nch" "nch_33" 
				    "nch_33_dnw" "nch_33_dnwx" "nch_33x" 
				    "nch_dnw" "nch_dnwx" "nch_hvt" "nch_hvt_dnw" 
				    "nch_hvt_dnwx" "nch_hvtx" "nch_lvt" 
				    "nch_lvt_dnw" "nch_lvt_dnwx" "nch_lvtx" 
				    "nch_na" "nch_na33" "nch_na33x" "nch_nax" 
				    "nchx" "pch" "pch_33" "pch_33x" "pch_hvt" 
				    "pch_hvtx" "pch_lvt" "pch_lvtx" "pchx" ) ))
    
    (setq selectedObjects (geGetSelSet)) ;; currentTopCellView)
    
    (if selectedObjects 
	(progn 
	  (printf "\n=======================================================\n")
	  (printf "List of transistors and their operation regions\n")
	  ;;     (printf "in %s.\n" currentTopCellView~>cellName)
	  (printf "=======================================================\n")
	  (printf " Cell Name       Transistor          Operation region\n")
	  (printf "-------------------------------------------------------\n")
	  (foreach singleObject selectedObjects 
		   (when (member singleObject~>cellName listOfTransistors)
		     (setq mathExp (strcat "OP(\"/" singleObject~>name "\",\"region\") "))
		     (setq hierPath (geGetInstHier))
		     (if (equal hierPath "/") (setq hierPath ""))
		     (setq mathExp (strcat "getData(\"" hierPath "/"  
					   singleObject~>name  
					   "\" ?result \"dcOpInfo-info\")~>region" ))
		     ;;		(printf "%s" mathExp)
		     (setq operationRegion (evalstring mathExp))
		     (if operationRegion 
			 (progn 
			   (setq operationRegion (nth (evalstring mathExp) regionLookUp)) ;; replace with assoc list instead.
			   (setq selectedTransistors (cons singleObject~>name selectedTransistors ))
			   (printf "%10s     %12s :        %s \n" hierPath singleObject~>name operationRegion)
			   )
		       (printf "%10s     %12s :        %s \n" singleObject~>name "Undefined!"))))
	  (printf "\n=======================================================\n"))
      )
    )  
  )

(defun asiValidVoltLevelp (voltLevel)		  
  ;; Used for short-cut key. Uses a file to temporarily store the 
  ;; data and then display it in a pop-up window.
  (let (selectedTransistors 
	operationRegion 
	selectedObjects 
	listOfTransistors
	bulkSource
	drainSource
	gateSource)       
    (setq selectedObjects (geGetSelSet)) ;; currentTopCellView    
    (if selectedObjects then
      (setq listOfTransistors (list "nch" "nch_33" "nch_33_dnw" "nch_33_dnwx" "nch_33x" "nch_dnw" "nch_dnwx" "nch_hvt" "nch_hvt_dnw" "nch_hvt_dnwx" "nch_hvtx" "nch_lvt" "nch_lvt_dnw" "nch_lvt_dnwx" "nch_lvtx" "nch_na" "nch_na33" "nch_na33x" "nch_nax" "nchx" "pch" "pch_33" "pch_33x" "pch_hvt" "pch_hvtx" "pch_lvt" "pch_lvtx" "pchx" ))
      (progn
	(setq oport (outfile (setq fileName (makeTempFileName "/tmp/validVolts"))))
	(if oport
	    (progn	
	      (fprintf oport "\n=============================================================\n")
	      (fprintf oport "DC simulation results from %s\n" (getCurrentWindow)-> cellView -> cellName)
	      (fprintf oport "List of transistors with their voltage levels\n")
	      (fprintf oport "=============================================================\n")
	      (fprintf oport " Cell Name       Transistor      State         Voltage levels\n")
	      (fprintf oport "-------------------------------------------------------------\n\n")
	      (foreach singleObject selectedObjects 
		       (when (member singleObject~>cellName listOfTransistors)
			 (setq hierPath (geGetInstHier))
			 (if (hierPath == "/") (setq hierPath ""))
			 
			 (setq compPath (strcat "getData(\"" hierPath "/"  singleObject~>name  "\" ?result \"dcOpInfo-info\")"))
			 
			 (setq mathExp (strcat compPath "~>vbs"))
			 (setq bulkSource -(evalstring mathExp))
			 (setq mathExp (strcat compPath "~>vds"))
			 (setq drainSource (evalstring mathExp))
			 (setq mathExp (strcat compPath "~>vgs"))
			 (setq gateSource (evalstring mathExp))
			 
			 (if (or bulkSource drainSource gateSource) then 
			   (fprintf oport "%10s     %12s :    " hierPath singleObject~>name)
		       (setq flag nil)
		       (if ((length (member singleObject~>cellName listOfTransistors)) > 2) 
			  (setq corr -1.0) 
			 (setq corr 1.0)) ; PMOS
		       ;; Primitive but OK
		       (if (corr * bulkSource > voltLevel) then
			  (fprintf oport "High VBS: %2.2f > %2.2f !\n" bulkSource voltLevel)
			  (setq flag t))
		       (if (corr * drainSource > voltLevel) then
			  (fprintf oport "High VDS: %2.2f > %2.2f !\n" drainSource voltLevel)
			  (setq flag t))
		       (if (corr * gateSource > voltLevel) then
			  (fprintf oport "High VGS: %2.2f > %2.2f !\n" gateSource voltLevel)
			  (setq flag t))
		       (if (not flag) (fprintf oport "OK!\n" ))
		    else
		       (fprintf oport ""))
		 ) ; ** when member **
	      ) ; ** foreach singleObject **
	      (fprintf oport  "\n=============================================================\n")
	      (close oport)
	      (view fileName nil (strcat "Voltage levels -- " (getCurrentTime) " -- " (getCurrentWindow)->cellView->cellName))
	      (deleteFile fileName)
	    ) ; ** progn **
	    (error "Couldn't open output file"))))  ))

(defun numax (object)  
  (while (or (famIsFamily object) (drIsWaveform object))
    (setq object (ymax object)))
  object)

(defun daisyXNL (values)
  (let ((maxINL 0) (maxDNL 0) (oldValue 0)
	(dnl (list )) (inl 0)
	(dnlOut (list )) (inlOut (list ))
	(avgDnl 0))
    
    (setq oldValue (car values))
    (foreach newValue (cdr values)
	     (setq dnl (append dnl (list newValue-oldValue)))
	     (setq avgDnl avgDnl+newValue-oldValue)
	     (setq oldValue newValue))
    (setq avgDnl avgDnl/(plus -1 (length values)))
    
    (foreach dnlValue dnl
	     (setq dnlOut (append dnlOut (list dnlValue-avgDnl)))
	     (setq inl inl+dnlValue-avgDnl)
	     (setq inlOut (append inlOut (list inl)))
	     (setq maxDNL
		   (max (abs dnlValue-avgDnl) maxDNL))
	     (setq maxINL
		   (max (abs inl) maxINL)))    
    (list (list "dnl" maxDNL) (list "inl" maxINL))))

(defun daisyHDmax (fftdB20 @optional (toneList (list 2 3 4 5)))
  (let ( (fVal 0) (distList nil) 
	 (distVal 0) (minDist 2200) 
	 (fundaPwr 0) (maxFreq (maxX fftdB20)))
    
    (setq fVal     (xmax (clip fftdB20 maxFreq/4096 maxFreq)))
    (setq fundaPwr (ymax (clip fftdB20 maxFreq/4096 maxFreq)))
    
    (foreach dist toneList
	 
	 (setq distVal (difference fundaPwr (value fftdB20 
						   (if (greaterp (times fVal dist) maxFreq) 
						       (difference maxFreq (times fVal dist))
						     (times fVal dist)))))
	 (if (lessp distVal minDist) (setq minDist distVal))
	 (setq distList (append (list distVal ) distList)))
    ;;(list fVal fundaPwr distList minDist)
    minDist))
