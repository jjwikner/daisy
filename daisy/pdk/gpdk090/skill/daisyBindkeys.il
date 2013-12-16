;; bindkeys


(printf "Adding special pdk specific bind keys...\n")

(if (not (boundp 'DAISYLISTOFBINDKEYS))
    (setq DAISYLISTOFBINDKEYS nil))


(if (not (boundp 'onlyOcean)) 
    (setq onlyOcean nil))

(if (not onlyOcean)
    (progn
      ;; ============= Moved to the pdk Specific area
      (daisySetBindKey  "Schematics" "CtrlShift<Key>n" "(schHiCreateInst \"gpdk090\" \"nmos1v\"  \"symbol\")" "Add an nmos transistor from vendor")
      (daisySetBindKey  "Schematics" "CtrlShift<Key>p" "(schHiCreateInst \"gpdk090\" \"pmos1v\"  \"symbol\")" "Add a  pmos transistor from vendor")
      
      ;; =============
      
      ))

