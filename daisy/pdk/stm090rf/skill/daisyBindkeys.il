;; bindkeys


(printf "Adding special pdk specific bind keys...\n")

(if (not (boundp 'DAISYLISTOFBINDKEYS))
    (setq DAISYLISTOFBINDKEYS nil))


(if (not (boundp 'onlyOcean)) 
    (setq onlyOcean nil))

(if (not onlyOcean)
    (progn
      ;; ============= Moved to the pdk Specific area
      
      (daisySetBindKey  "Layout" "Alt<Key>1" "(daisyToggleDFMAnalog)" "Toggles the DFM analog mode of tsmc transistors.")
      (daisySetBindKey  "Layout" "Alt<Key>2" "(daisyTogglePolyRoute)" "Toggles the poly route position on tsmc transistors.")
      (daisySetBindKey  "Layout" "Alt<Key>3" "(daisyTogglePolyContacts)" "Toggles the poly contacts on tsmc transistors.")

      (daisySetBindKey  "Layout" "Alt<Key>U" "(daisyRotateLayers (geGetSelSet) t)" "Up/Dn toggle.")
      (daisySetBindKey  "Layout" "Alt<Key>D" "(daisyRotateLayers (geGetSelSet) nil)" "Up/Dn toggle.")

      (daisySetBindKey  "Layout" "AltShift<Key>u" 
			"(leDisplaySomeLayersOnly (list (list \"M1\" \"drawing\")
			   (list \"M2\" \"drawing\")
			   (list \"M3\" \"drawing\")
			   (list \"M4\" \"drawing\")
			   (list \"M5\" \"drawing\")
			   (list \"M6\" \"drawing\")
			   (list \"M7\" \"drawing\")
			   (list \"M8\" \"drawing\")
			   (list \"M9\" \"drawing\")
			   (list \"M10\" \"drawing\")
			   (list \"text\" \"drawing\"))"
			"Display metal and text only")
      
      (daisySetBindKey  "Layout" "<Key>1" "(setGridXY \"xy\"  0.01  0.04 8)" "Set grid to 0.005")
      (daisySetBindKey  "Layout" "<Key>2" "(setGridXY \"xy\"  0.14   0.80 8)" "Set grid to 0.14")
      (daisySetBindKey  "Layout" "<Key>3" "(setGridXY \"xy\"  0.28   1.60 8)" "Set grid to 0.28")
      (daisySetBindKey  "Layout" "<Key>4" "(setGridXY \"xy\"  0.56   3.20 8)" "Set grid to 0.56")
      (daisySetBindKey  "Layout" "<Key>5" "(setGridXY \"xy\"  1.12  16.00 8)" "Set grid to 1.12")
      (daisySetBindKey  "Layout" "<Key>6" "(setGridXY \"xy\"  2.52  32.00 8)" "Set grid to 2.52")
      (daisySetBindKey  "Layout" "<Key>7" "(setGridXY \"xy\"  5.04  64.00 8)" "Set grid to 5.04")
      (daisySetBindKey  "Layout" "<Key>8" "(setGridXY \"xy\" 10.08 100.00 10)" "Set grid to 10.08")

      ;; Align to umcs pad pitch.
      (daisySetBindKey  "Layout" "<Key>9" "(setGridXY \"xy\" 35.00 140.00 5)" "Set grid to 35")
      (daisySetBindKey  "Layout" "<Key>0" "(setGridXY \"xy\" 30.00 120.00 6)" "Set grid to 30")
      ;;
           
      (daisySetBindKey  "Schematics" "CtrlShift<Key>n" "(schHiCreateInst \"cmos090\" \"nsvt\"  \"symbol\")" "Add an nmos transistor from vendor")
      (daisySetBindKey  "Schematics" "CtrlShift<Key>p" "(schHiCreateInst \"cmos090\" \"psvt\"  \"symbol\")" "Add a  pmos transistor from vendor")
      
      ;; =============
      
      ))

