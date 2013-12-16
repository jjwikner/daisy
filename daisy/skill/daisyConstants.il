(printf " ... loading %s.il ...\n" "constMods")

;;
;; Definition of constants 
;;

(defun kT (@optional (T 27) (scale "C"))
  ;; Returns the kT constant
  (let ((offset 271.2))
    
    (if (equal scale "K")
	(setq offset 0))
    (times 1.381e-23 (plus T offset))))


(defun kTC (cap @optional (T 27) (scale "C"))
  ;; Returns the *RMS* noise voltage for a given capacitance C.
  (sqrt (quotient (kT T scale) cap)))

(setq PI 3.1415926535897932384626433832795)
