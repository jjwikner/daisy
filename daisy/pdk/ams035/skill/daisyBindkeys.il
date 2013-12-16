;; bindkeys
(daisyLog "... adding special pdk specific bind keys...\n")

(if (not (boundp 'DAISYLISTOFBINDKEYS))
    (setq DAISYLISTOFBINDKEYS nil))

(if (not (boundp 'onlyOcean)) 
    (setq onlyOcean nil))

(if (not onlyOcean)
    (progn
      ;; ============= Moved to the pdk Specific area
      (daisySetBindKey  "Layout" "AltShift<Key>u" 
			"(leDisplaySomeLayersOnly (list 
			   (list \"M1\" \"drawing\")
			   (list \"M2\" \"drawing\")
			   (list \"M3\" \"drawing\")
			   (list \"M4\" \"drawing\")
			   (list \"text\" \"drawing\"))"
			"Display metal and text only")
           
      (daisySetBindKey  "Schematics" "CtrlShift<Key>n" "(schHiCreateInst \"PRIMLIB\" \"nmos4\"  \"symbol\")" "Add an nmos transistor from vendor")
      (daisySetBindKey  "Schematics" "CtrlShift<Key>p" "(schHiCreateInst \"PRIMLIB\" \"pmos4\"  \"symbol\")" "Add a  pmos transistor from vendor")
      
      ;; =============
      
      ))

