;; This file contains useful functions for data manipulation
;;

(printf " ... loading %s.il ...\n" "mathMods")


(defun daisyNearestPrime (NUM)
  ;; Christer Jansson
  (let (NUMR prime primSmall primLarge NUMLOOP)
    
    (setq NUMR  (round NUM))
    
    (if (daisyCheckIfPrime NUMR)  then
      prime = NUMR
      else
      
      (setq prime nil)
      (setq NUMLOOP NUMR)
      (while (and (not prime)  (greaterp NUMLOOP 2))
	NUMLOOP--
	(setq prime (daisyCheckIfPrime NUMLOOP))
	) ;while
      primSmall = NUMLOOP
      
      prime = nil
      NUMLOOP = NUMR
      (while (not prime)
	NUMLOOP++
	(setq prime (daisyCheckIfPrime NUMLOOP))
	) ;while
      (setq primLarge NUMLOOP)
      
      (if (abs(primLarge-NUM)<abs(primSmall-NUM)) then
	prime = primLarge
	else
	prime = primSmall
	) ;if
      ) ; if
    prime
    ) ; let
  ) ; defun


(defun daisyCheckIfPrime (NUT)
;; Christer Jansson
  let((prime b div)
      prime = t
      b=1+floor(sqrt(NUT))
      while( (prime&&(b!=1))
	     div = float(NUT)/float(b)
	     when((abs(div-round(div))<1e-9)
		  prime = nil      
		  )
	     b--
	     )
      prime
      )
  )


(defun daisySum (p1 p2)				  
  ;; generates the sum of two two-dimensional points.
  
  (if (and (listp p1) (listp p2))
      (if (and p1 p2)
	  (cons
	   (daisySum (car p1) (car p2))
	   (daisySum (cdr p1) (cdr p2))))
    (plus p1 p2)))

(defun daisyDiff (p1 p2)				  
  ;; Generates the difference between two points.
  (if (and (listp p1) (listp p2))
      (if (and p1 p2)
	  (cons
	   (daisyDiff (car p1) (car p2))
	   (daisyDiff (cdr p1) (cdr p2))))
    (difference p1 p2)))

(defun daisyMult (p1 p2)				  
  ;; Multiplies two points.
  (if (and (listp p1) (listp p2))
      (if (and p1 p2)
	  (cons
	   (daisyMult (car p1) (car p2))
	   (daisyMult (cdr p1) (cdr p2))))
    (mult p1 p2)))

(defun daisyPackSack (n_total l_weights)		    
;;; The algorithm searches for the combination of weights that 
;;; produces a sum as close to n_total as possible. The input 
;;; list with weights is sorted and the returned list is sorted 
;;; with the largest weight first.
  (let ((l_sWeights l_weights)
	(alreadyFound nil)
	(l_cWeights nil)
	(minWeight 1)
	(l_urban nil))
    (setq l_urban l_weights)
    (setq l_sWeights (sort l_urban 'greaterp))
    (setq minWeight (car (last l_sWeights)))
    (while (or (greaterp n_total minWeight) (equal n_total minWeight))
      (setq alreadyFound nil)       
      (foreach weight l_sWeights
	       (if (and (n_total >= weight) (not alreadyFound)) then
		 (setq l_cWeights (cons weight l_cWeights))
		 (setq alreadyFound t)
		 (setq n_total (n_total - weight)))))
    (reverse l_cWeights)))

(defun daisyCenterPoint (l_bBox @optional (grid nil))
  (if (not grid) (setq grid 1))
  (list (float grid*(round ((caar l_bBox)+(caadr l_bBox))/2.0/grid))
	(float grid*(round ((cadar l_bBox)+(cadadr l_bBox))/2.0/grid))))

(defun daisyRectCenterPoint (l_bBox @optional (grid nil))
  ;; Returns the center point of a box
  (daisyCenterPoint l_bBox grid))

(defun daisyRectWidth (l_bBox)
  ;; Width of e.g. bBox
  (abs (difference (caadr l_bBox) (caar l_bBox))))

(defun daisyRectHeight (l_bBox)
  ;; Height of e.g. bBox
  (abs (difference (cadadr l_bBox) (cadar l_bBox))))

(defun daisySaruman (decimalNumber @optional (bits 8))
  ;; SAR generation of bits in an ADC provided a decimal number
  (let (( ref (pow 2 (bits-1))) (result nil) (bitCounter 0))
    
    (while (lessp bitCounter bits)      
      (if (or (greaterp decimalNumber ref) (equal decimalNumber ref))
	  (progn
	    (setq result (append result (list 1)))
	    (setq decimalNumber (times 2 (difference decimalNumber ref)))
	    )	
	(progn
	  (setq result (append result (list 0)))
	  (setq decimalNumber (times 2.0 decimalNumber))
	  )	
	)
      (setq bitCounter (plus bitCounter 1)))
    result))
    
