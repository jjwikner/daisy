;; bindkeys


(printf "Adding special pdk specific bind keys...\n")

(if (not (boundp 'DAISYLISTOFBINDKEYS))
    (setq DAISYLISTOFBINDKEYS nil))

;; This you probably want to set with a generic function instead, to which you list the different purposes and associate them with the layerNames
(if (not (boundp 'GLOBALLAYERPAIRLIST))
    (setq GLOBALLAYERPAIRLIST 
	  (list  
	   (list "Metal1" "drawing")  
	   (list "Metal2" "drawing") 
	   (list "Metal3" "drawing") 
	   (list "Metal4" "drawing") 
	   (list "Metal5" "drawing")
	   (list "Metal6" "drawing")
	   (list "text" "drawing"))))

(daisySetBindKey  "Layout" "Alt<Key>U" "(daisyRotateLayers (geGetSelSet) t)" "Up/Dn toggle.")
(daisySetBindKey  "Layout" "Alt<Key>D" "(daisyRotateLayers (geGetSelSet) nil)" "Up/Dn toggle.")

(daisySetBindKey  "Layout" "AltShift<Key>u" 
		  "(leDisplaySomeLayersOnly (list (list \"Metal1\" \"drawing\")
			   (list \"Metal2\" \"drawing\")
			   (list \"Metal3\" \"drawing\")
			   (list \"Metal4\" \"drawing\")
			   (list \"Metal5\" \"drawing\")
			   (list \"Metal6\" \"drawing\")
			   (list \"Metal7\" \"drawing\")
			   (list \"Metal8\" \"drawing\")
			   (list \"Metal9\" \"drawing\")
			   (list \"Metal10\" \"drawing\")
			   (list \"text\" \"drawing\"))"
			"Display metal and text only")

(daisySetBindKey  "Layout" "<Key>1" "(setGridXY \"xy\"  0.005  0.04 8)" "Set grid to 0.005")
(daisySetBindKey  "Layout" "<Key>2" "(setGridXY \"xy\"  0.1   0.40 8)" "Set grid to 0.1")
(daisySetBindKey  "Layout" "<Key>3" "(setGridXY \"xy\"  0.2   1.6 8)" "Set grid to 0.2")
(daisySetBindKey  "Layout" "<Key>4" "(setGridXY \"xy\"  0.4   3.20 8)" "Set grid to 0.4")
(daisySetBindKey  "Layout" "<Key>5" "(setGridXY \"xy\"  0.8   6.4 8)" "Set grid to 0.8")
(daisySetBindKey  "Layout" "<Key>6" "(setGridXY \"xy\"  2.6  32.00 8)" "Set grid to 2.6")
(daisySetBindKey  "Layout" "<Key>7" "(setGridXY \"xy\"  5.2  64.00 8)" "Set grid to 5.2")
(daisySetBindKey  "Layout" "<Key>8" "(setGridXY \"xy\" 10.4 100.00 10)" "Set grid to 10.4")

;; Align to umcs pad pitch.
(daisySetBindKey  "Layout" "<Key>9" "(setGridXY \"xy\" 35.00 140.00 5)" "Set grid to 35")
(daisySetBindKey  "Layout" "<Key>0" "(setGridXY \"xy\" 30.00 120.00 6)" "Set grid to 30")
;;

(if (not (boundp 'onlyOcean)) 
    (setq onlyOcean nil))

(if (not onlyOcean)
    (progn
      ;; ============= Moved to the pdk Specific area
      (daisySetBindKey  "Schematics" "CtrlShift<Key>n" "(schHiCreateInst \"gpdk045\" \"nmos1v\"  \"symbol\")" "Add an nmos transistor from vendor")
      (daisySetBindKey  "Schematics" "CtrlShift<Key>p" "(schHiCreateInst \"gpdk045\" \"pmos1v\"  \"symbol\")" "Add a  pmos transistor from vendor")
      ;; =============
      
      ))

