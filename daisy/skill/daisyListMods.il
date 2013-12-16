(setq currentModule "daisyListMods")

;;======================

(printf " ... loading %s ... \n"
	currentModule )

(defun daisyCreateStates (initStates startVal stopVal incrVal @rest specPoints)

  ;; 
  ;;
  ;;
  
  (let ( (newStates nil)  
	 (paramVal startVal) )
    
    (if (not initStates)  ; List is empty           
	(progn 
	  (if startVal
	      (while (not (greaterp paramVal stopVal+1e-13))
		(setq newStates (cons paramVal newStates))
		(setq paramVal (plus paramVal incrVal))
		;; (printf "%f\n" (float paramVal))
		))
	  (if specPoints
	      (foreach newPoint (car specPoints)
		       ;;       (printf "%f\n" newPoint*1.0)
		       (setq newStates (cons newPoint newStates)))))
      
      (progn
	(foreach subState initStates
		 (setq paramVal startVal)
		 (if startVal
		     (while (not (greaterp paramVal stopVal+1e-13))
		       (if (listp subState)
			   (setq newStates (cons (cons paramVal subState)         newStates))
			 (setq newStates (cons (cons paramVal (list subState))  newStates)))
		       (setq paramVal (plus paramVal incrVal))))
		 (if specPoints
		     (foreach newPoint (car specPoints)
			      (if (listp subState)
				  (setq newStates (cons (cons newPoint subState)         newStates))
				(setq newStates (cons (cons newPoint (list subState))  newStates))))))))
    (reverse newStates)))


(defun daisyExpandStatesN (dataParams)
  
  
  ;; Input dataParams given by
  ;; ( (identifier_1 startVal_1 stopVal_1 incrVal_1 @rest ()  )  
  ;;   (identifier_2 startVal_2 stopVal_2 incrVal_2 @rest ()  ) 
  ;;    ...
  ;;   (identifier_n startVal_n stopVal_n incrVal_n @rest ()  )
  
  (let ( ( explodeStates nil )
	 ( explodeIdents nil ) )
    
    (foreach subData (reverse dataParams)
	     (setq explodeStates (daisyCreateStates explodeStates 
						    (cadr   subData) 
						    (caddr  subData) 
						    (cadddr subData)
						    (cddddr subData))))
    
    (foreach subData (reverse dataParams)
	     (setq explodeIdents (cons (car subData) explodeIdents)))
    (cons explodeIdents explodeStates)))



(defun daisyExpandStates (dataParams)
  (let ((simList (daisyExpandStatesN dataParams)) 
	(bundleNames (list)) (bundleList nil) 
	(bundleName nil) (bundleList1 nil))
    
    (setq bundleNames (car simList))
    ;;(printf "%L " simList)
    (foreach bundle (cdr simList)
	     (setq bundleName  bundleNames)
	     (setq bundleList1 nil)
	     (foreach bundleItem bundle		      
		      (setq bundleList1 (append bundleList1 
						(list (list (car bundleName) bundleItem))))
		      (setq bundleName (cdr bundleName)))
	     (setq bundleList (append bundleList  (list bundleList1))))
    bundleList))

(defun daisyTestStates ()
  (let ( (l_varVals
	  (daisyExpandStates 
	   (list 
	    (list "DVDD_DC" 1.5 1.7 0.2)
	    (list "corner"  nil nil nil "slow" "nom" "fast")
	    (list "AVDD_DC" 3.0 3.6 0.3 )))) )  
    
    (foreach vals (cdr l_varVals)
	     (mapcar 
	      `(lambda (variable value) 
		 (cond
		  ( (equal variable "temp")
		    (temp value*1.0)
		    (printf "Setting temperature to %f.\n" value*1.0) )
		  ( (equal variable "corner")
		    (modelFile (list "include.scs"  (sprintf nil "%s" value))) 
		    (printf "Setting model file to %s. \n" value) )
		  (t
		   (desVar variable value))))
	      (car l_varVals) vals))))

(defun lMax (l_vals)	
  (car (sort l_vals 'greaterp)))

(defun lMin (l_vals)	
  (car (sort l_vals 'lessp)))


(defun parseIteratedInstance (t_numbering @optional (sortOrder nil))
  ;; _Oct  7 04 wikner 397
  
  ;; Parses a string that describes the numbering of an iterated
  ;; instance and returns a list in decimal numbers.

  ;; Notice: This function ignores the order!
  
  (let (l_subStrings l_numbers newSubStr)    
    (if t_numbering 
	(progn
	  (setq l_subStrings (parseString t_numbering "<,>"))       
	  (foreach subStr (reverse l_subStrings)
		   (setq newSubStr (parseString subStr ":"))	  
		   (if (greaterp (length newSubStr) 1)   ; Separated by : 	     
		       (if (greaterp (evalstring (car newSubStr)) (evalstring (cadr newSubStr)))		
			   (for index (evalstring (cadr newSubStr)) (evalstring (car newSubStr))
				(setq l_numbers (cons index l_numbers)))
			 (progn
			   (for index (evalstring (car newSubStr)) (evalstring (cadr newSubStr))
				(setq l_numbers (cons index l_numbers)))
			   (setq l_numbers (reverse l_numbers))))
		     (setq l_numbers (cons (evalstring (car newSubStr)) l_numbers))))
	  ;;(setq l_numbers (reverse l_numbers))
	  ))
    (if sortOrder (sort l_numbers sortOrder) l_numbers) ))

(defun daisyIterSplit (t_numbering)  
  ;; Parses a string that describes the numbering of an iterated
  ;; instance and returns a list in decimal numbers.
  
  ;; It is assumed that the iterated string is a valid database identifier.
  (let ( ( l_subStrings (list ))
	 ( l_numbers (list ))
	 (newSubStr "")
	 (l_subnumbers  (list))	 
	 ( l_result nil) )
    
    (if t_numbering 
	(progn
	  (setq l_subStrings (parseString t_numbering "<,>"))
	  
	  ;; The first string will be the instance name
	  ;; the rest are the iterations

	  (foreach subStr  (cdr l_subStrings)
		   
		   (setq l_subnumbers (list))
		   
		   (setq newSubStr (parseString subStr ":"))	  
		   		   
		   (if (greaterp (length newSubStr) 1)   
		       ;; The segment was eparated by :
		       (if (greaterp (evalstring (car newSubStr)) (evalstring (cadr newSubStr)))
			   ;; If the first index was smaller than the second
			   
			   (for index (evalstring (cadr newSubStr)) 
				(evalstring (car newSubStr))
				(setq l_subnumbers (append l_subnumbers (list index))))
			 
			 (progn
			   ;; else it is the other way around.
			   (for index (evalstring (car newSubStr)) (evalstring (cadr newSubStr))
				(setq l_subnumbers (cons index l_subnumbers)))
			   ))

		     (setq l_subnumbers (cons (evalstring (car newSubStr)) l_subnumbers)))
		   (setq l_numbers (append l_numbers (reverse l_subnumbers))))

	  (if l_numbers
	      (progn
		(foreach l_number l_numbers
			 (setq l_result (append l_result (list (sprintf nil "%s<%d>" (car l_subStrings) l_number)))))
		)
	    (setq l_result l_subStrings))
	  )
      )

    l_result))


(defun daisyList2String (theList)
  
  (let ((stringOut ""))
    (foreach element theList
	     (setq stringOut (strcat element " " stringOut)))
    stringOut))

(defun assoCadr (assocList searchKey)  
  (cadr (assoc searchKey assocList)))


(defun daisyUniqueList (inputList)
  (let ( ( outputList nil ))
    (foreach element inputList 
	     (if (not (member element outputList))
		 (setq outputList (append outputList (list element)))))
    outputList))
