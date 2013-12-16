;; Daisy Flow Bindkeys

(printf " ... loading %s.il ...\n" "daisyBindkeys")

;;

(printf "Adding special bind keys...\n")

(setq DAISYLISTOFBINDKEYS nil)

(defun daisySetBindKey (tool key command @optional (comment "NO COMMENT YET :("))
  (setq DAISYLISTOFBINDKEYS 
	(append (list (list 
		       (case tool 
			     ((equal "Command Interpreter") "CIW") 
			     ((equal "Abstract Editor") "Abstract")  
			     ((equal "awv") "Waveform") 
			     (else tool))  
		       key 
		       comment) ) 
		DAISYLISTOFBINDKEYS))  
  (hiSetBindKey tool key command))

(defun daisyPrintBindKeys ()
  (printf "Daisy list of bindkeys \n")
  (printf "====================== \n")
  (foreach keycombo DAISYLISTOFBINDKEYS
	   (printf "%12s  %18s %s \n" (car keycombo) (cadr keycombo) (caddr keycombo))) t)

(if (not (boundp 'onlyOcean)) (setq onlyOcean nil))

(if (not onlyOcean)
    (progn
     ;; Symbol view
     (daisySetBindKey  "Symbol"     "Ctrl<Key>0"     "(daisyGridObjectXY \"xy\" 1)" "Align objects to current grid")
     (daisySetBindKey  "Symbol"     "Alt<Key>a"      "(daisySchAnnotatePin)"           "Add pin cdf information to symbol to be able to see sim results" )
     (daisySetBindKey  "Symbol"     "Alt<Key>t"      "(daisyDeCypress (hiGetCurrentWindow)->cellView)" "Remove all properties from a symbol")
     (daisySetBindKey  "Symbol"     "Alt<Key>r"      "(daisyAddSymbolName (hiGetCurrentWindow)->cellView t)" "Adds symbol text info")
     (daisySetBindKey  "Symbol"     "Shift<Key>z"    "(hiZoomRelativeScale (hiGetCurrentWindow) 0.5)" "Zoom out in symbol" )
     (daisySetBindKey  "Symbol"     "AltShift<Key>j" "(daisySchPlot)" "Plot symbol to file")
     
     ;; Schematics view
     (daisySetBindKey  "Schematics" "Ctrl<Key>b"  "(daisyCheckBulkConnNMOS)" "For STM065,Check if bulk of any NMOS is not connected to ground node in the selected schematic")
     (daisySetBindKey  "Schematics" "<Key>."  "(daisyUpdateFingerM)" "Updates the CDF parameters so that finger and multiplier are 1 without destroying callback functions.")
     (daisySetBindKey  "Schematics" "Shift<Key>>"  "(daisyToggleImportantNote)" "Toggle the status of a text box to be important or not.")

     (daisySetBindKey  "Schematics" "<Key>,"  "(daisyReportFingerM)" "Prints the mult and finger of different instances of the schematic.")
     
     (daisySetBindKey  "Schematics" "<Key>F3" "(daisyOpenSomeCells)" "Opens the schematic and layout views of selected object")
     (daisySetBindKey  "Layout" "<Key>F3" "(daisyOpenSomeCells)" "Opens the schematic and layout views of selected object")

     (daisySetBindKey  "Schematics" "Ctrl<Key>0"  "(daisyGridObjectXY \"xy\" 1)" "Align objects to current grid")
     (daisySetBindKey  "Schematics" "Ctrl<Key>1"  "(asiPresentRegions)" "Given simulation results, the transistor operation regions are displayed")
     (daisySetBindKey  "Schematics" "Ctrl<Key>2"  "(asiAnnotateDesignVariables)" "Given simulation results, the design variables are added to the schematic")
     (daisySetBindKey  "Schematics" "Ctrl<Key>3"  "(daisyPresentRegionsPopUp)" "Given simulation results, the transistor operation regions are displayed in pop-up window")
     (daisySetBindKey  "Schematics" "Ctrl<Key>4"  "(asiValidVoltLevelp 1.6)" "Given simulation results, transistor violating a 1.6-V gate-to-drain/source limit are indicated")
     (daisySetBindKey  "Schematics" "Ctrl<Key>5"  "(asiAnnotateNetVoltage)" "Given simulation results, net voltages are printed.")

     

     (daisySetBindKey  "Schematics" "<Key>1"  "sevNetlistAndRun((asiGetCurrentSession)->data -> sevSession)" "Runs the simulation.")
     (daisySetBindKey  "Schematics" "<Key>2"  "sevDirectPlot((asiGetCurrentSession)->data -> sevSession 'asiDirectPlotResultsMenuCB)" "Opens the main form for plotting results.")
     (daisySetBindKey  "Schematics" "<Key>4"  "daisyOpenHyperLink((car (geGetSelSet)))" "Opens a web browser with the hyperlink found in a label/text box.")
     (daisySetBindKey  "Schematics" "<Key>3"  "daisyToggleAnnotate()" "Toggles the annotation layer for pretty printing - requires however the LSW to be opened :(.")
     
     (daisySetBindKey  "Schematics" "Ctrl<Key>9"  "(geDeleteAllProbe (getCurrentWindow) t)" "Remove all probes")
     (daisySetBindKey  "Schematics" "Shift<Key>z" "(hiZoomRelativeScale (hiGetCurrentWindow) 0.8)" "Zoom out as in layout")
     (daisySetBindKey  "Schematics" "<Key>f"      "(schZoomFit 0.9 0.9)" "Zoom to fit as in layout")
     (daisySetBindKey  "Schematics" "AltShift<Key>l" "(daisySchReportProbes)" "Go through schematic hieararchy and find all cells that are circuit probes" )
     (daisySetBindKey  "Schematics" "<Key>F2"      "(daisyReportPorts)" "Return the ports of a component in a csv file and launching openoffice")
     (daisySetBindKey  "Schematics" "<Key>F7"      "(TREEview (hiGetCurrentWindow)->cellView t)" "Prints the hierarchy.")
     
     ;; Check and save
     (daisySetBindKey  "Schematics" "Shift<Key>y" "(let ((cv (hiGetCurrentWindow)->cellView) (ccv (geGetTopLevelCellView (hiGetCurrentWindow)))) (schCheckHier ccv \"schematic symbol verilog functional veriloga\" daisyList2String((ddGetLibList)~>name) ) (artGenerateHierSymbolCDF cv t))" "Check schematics hiearchy prior to simulation. Also updates the CDF parameters.")
     ;; artGenerateHierSymbolCDF(hiGetCurrentWindow()->cellView)
     ;; Bindkeys for adding special instances
     (daisySetBindKey  "Schematics" "CtrlShift<Key>t" "(schHiCreateInst \"basic\"     \"noConn\"  \"symbol\")" "Add a terminator from basic lib")
     (daisySetBindKey  "Schematics" "CtrlShift<Key>r" "(schHiCreateInst \"analogLib\" \"res\"     \"symbol\")" "Add a resistor from analogLib")
     (daisySetBindKey  "Schematics" "CtrlShift<Key>l" "(schHiCreateInst \"analogLib\" \"ind\"     \"symbol\")" "Add an inductor from analogLib" )
     (daisySetBindKey  "Schematics" "CtrlShift<Key>c" "(schHiCreateInst \"analogLib\" \"cap\"     \"symbol\")" "Add a capacitor from analogLib")
     (daisySetBindKey  "Schematics" "Shift<Key>c" "(schHiCreateInst \"analogLib\" \"cap\"     \"symbol\")" "Add a capacitor from analogLib (Cadence 6)")
     (daisySetBindKey  "Schematics" "CtrlShift<Key>v" "(schHiCreateInst \"analogLib\" \"vdc\"     \"symbol\")" "Add a voltage source from analogLib")
     (daisySetBindKey  "Schematics" "CtrlShift<Key>s" "(schHiCreateInst \"analogLib\" \"vsin\"    \"symbol\")" "Add a sinusoidal source from analogLib")
     (daisySetBindKey  "Schematics" "CtrlShift<Key>u" "(schHiCreateInst \"analogLib\" \"vpulse\"    \"symbol\")" "Add a pulse source from analogLib")
     (daisySetBindKey  "Schematics" "CtrlShift<Key>e" "(schHiCreateInst \"analogLib\" \"idc\"     \"symbol\")" "Add a current source from analogLib")
     (daisySetBindKey  "Schematics" "CtrlShift<Key>g" "(schHiCreateInst \"analogLib\" \"gnd\"     \"symbol\")" "Add a global ground symbol from analogLib")
     (daisySetBindKey  "Schematics" "CtrlShift<Key>d" "(schHiCreateInst \"analogLib\" \"vdd\"     \"symbol\")" "Add a global supply symbol from analogLib")
     (daisySetBindKey  "Schematics" "CtrlShift<Key>q" "(schHiCreateInst \"daisy\" \"daisySheet\"  \"symbol\")" "Add a sheet symbol")

     (daisySetBindKey  "Schematics" "<Key>F5" "(sh (sprintf nil \"emacs %s &\" (caaddr (asiGetCurrentSession)->data->netlistStatus)))" "Displays the netlist of the current session in the Emacs editor.")

     
     (daisySetBindKey  "Schematics" "AltShift<Key>j" "(daisySchPlot)" "Plot schematic")
     (daisySetBindKey  "Schematics"     "AltShift<Key>k" "(daisySchPlot ?destLib \"./doc/figs/\")" "Plot schematic to file")
     (daisySetBindKey  "Schematics" "Alt<Key>s" "(daisyAddSupplyNames)" "Add supply names to selected or all objects")
     
     ;; Layout callbacks
     
     (daisySetBindKey  "Layout" "<Key><"      "(daisyReportInfo nil)" "Reports information on the selected objects.")
     (daisySetBindKey  "Layout" "Shift<Key>>" "(daisyFillArea ?fillBoundary (car (geGetSelSet)))" "Fills an object with another object.")
     

;;     (daisySetBindKey  "Layout" "Alt<Key>1" "(daisyToggleDFMAnalog)" "Toggles the DFM analog mode of tsmc transistors.")
;;     (daisySetBindKey  "Layout" "Alt<Key>2" "(daisyTogglePolyRoute)" "Toggles the poly route position on tsmc transistors.")
;;     (daisySetBindKey  "Layout" "Alt<Key>3" "(daisyTogglePolyContacts)" "Toggles the poly contacts on tsmc transistors.")
     ;; (daisySetBindKey  "Layout" "<Key>F3"      "(daisyPrintPins)" "Return the pins in layout and launch openoffice")

     (daisySetBindKey "Layout" "Alt<Key>l" "(daisyAddLabel)" "Adds a label to a pin")

     (daisySetBindKey "Layout" "<Key>h" "(daisyPinny)" "Creates a pin out of a rect and label.")
     (daisySetBindKey  "Layout" "Shift<Key>1" "(alignAverageXY \"x\")" "Align objects in the x-direction to their average")
     (daisySetBindKey  "Layout" "Shift<Key>2" "(alignAverageXY \"y\")" "Align objects in the y-direction to their average")
     (daisySetBindKey  "Layout" "Shift<Key>3" "(mirrorXY \"x\")" "Mirror objects in the x-direction")
     (daisySetBindKey  "Layout" "Shift<Key>4" "(mirrorXY \"y\")" "Mirror objects in the y-direction")
     (daisySetBindKey  "Layout" "Shift<Key>5" "(distributeXY \"x\")" "Distribute the objects in the x-direction to be equidistant")
     (daisySetBindKey  "Layout" "Shift<Key>6" "(distributeXY \"y\")" "Distribute the objects in the y-direction to be equidistant")
     (daisySetBindKey  "Layout" "Shift<Key>7" "(daisySquareDance)" "Distribute the objects in the x- and y-direction to be square")
     (daisySetBindKey  "Layout" "AltShift<Key>7" "(daisySquareDance nil)" "Distribute the objects in the x- and y-direction to be square")
     (daisySetBindKey  "Layout" "Shift<Key>8" "(daisyTranspose)" "Transpose the position of the cells")
     (daisySetBindKey  "Layout" "Ctrl<Key>b"  "(daisyCopyBoundary)" "Creates an instance layer as a copy of existing boundary layer")
     (daisySetBindKey  "Layout" "g" "(printf \"REMOVED THIS ANNOYING FUNCTION!\")")
     
     (daisySetBindKey  "Layout" "Alt<Key>s" "(daisySwapLocations )" "Select two objects and swap their rotation and location with this key.")

     (daisySetBindKey  "Layout" "Ctrl<Key>1" "(daisyGridObjectXY \"xy\" 2)"   "Align object to 2x grid" )
     (daisySetBindKey  "Layout" "Ctrl<Key>2" "(daisyGridObjectXY \"xy\" 4)"   "Align object to 4x grid")
     (daisySetBindKey  "Layout" "Ctrl<Key>3" "(daisyGridObjectXY \"xy\" 8)"   "Align object to 8x grid")
     (daisySetBindKey  "Layout" "Ctrl<Key>4" "(daisyGridObjectXY \"xy\" 16)"  "Align object to 16x grid")
     (daisySetBindKey  "Layout" "Ctrl<Key>5" "(daisyGridObjectXY \"xy\" 32)"  "Align object to 32x grid")
     (daisySetBindKey  "Layout" "Ctrl<Key>6" "(daisyGridObjectXY \"xy\" 20)"  "Align object to 20x grid")
     (daisySetBindKey  "Layout" "Ctrl<Key>7" "(daisyGridObjectXY \"xy\" 100)" "Align object to 100x grid")
     (daisySetBindKey  "Layout" "Ctrl<Key>8" "(daisyGridObjectXY \"xy\" 200)" "Align object to 200x grid")
     (daisySetBindKey  "Layout" "Ctrl<Key>9" "(daisyGridObjectXY \"xy\" 400)" "Align object to 400x grid")
     (daisySetBindKey  "Layout" "Ctrl<Key>0" "(daisyGridObjectXY \"xy\" 1)"   "Align object to 1x grid")

     (daisySetBindKey  "Layout" "Alt<Key>Left"       "(let  ((cw (hiGetCurrentWindow))) (daisyMoveObjectXY \"x\" -cw->xSnapSpacing))" "Move object 1x grid to left")
     (daisySetBindKey  "Layout" "AltShift<Key>A"     "(let  ((cw (hiGetCurrentWindow))) (daisyPrintArea )" "Reports the Area of the current cellview")
     (daisySetBindKey  "Layout" "Alt<Key>Up"         "(let  ((cw (hiGetCurrentWindow))) (daisyMoveObjectXY \"y\"  cw->ySnapSpacing))" "Move object 1x grid to up")
     (daisySetBindKey  "Layout" "Alt<Key>Down"       "(let  ((cw (hiGetCurrentWindow))) (daisyMoveObjectXY \"y\" -cw->ySnapSpacing))" "Move object 1x grid to down")
     (daisySetBindKey  "Layout" "Alt<Key>Right"      "(let  ((cw (hiGetCurrentWindow))) (daisyMoveObjectXY \"x\"  cw->xSnapSpacing))" "Move object 1x grid to right")
     (daisySetBindKey  "Layout" "CtrlShift<Key>Up"   "(let  ((cw (hiGetCurrentWindow))) (daisyGrowObjectXY \"up\"))" "Grow object")
     (daisySetBindKey  "Layout" "CtrlShift<Key>Down" "(let  ((cw (hiGetCurrentWindow))) (daisyGrowObjectXY \"dn\"))" "Shrink object")
     
     (daisySetBindKey  "Layout" "AltShift<Key>Left"  "(let  ((cw (hiGetCurrentWindow))) (daisyMoveObjectXY \"x\" -4*(cw->xSnapSpacing)))" "Move object 4x grid to left")
     (daisySetBindKey  "Layout" "AltShift<Key>Up"    "(let  ((cw (hiGetCurrentWindow))) (daisyMoveObjectXY \"y\"  4*(cw->ySnapSpacing)))" "Move object 4x grid to up")
     (daisySetBindKey  "Layout" "AltShift<Key>Down"  "(let  ((cw (hiGetCurrentWindow))) (daisyMoveObjectXY \"y\" -4*(cw->ySnapSpacing)))" "Move object 4x grid to down")
     (daisySetBindKey  "Layout" "AltShift<Key>Right" "(let  ((cw (hiGetCurrentWindow))) (daisyMoveObjectXY \"x\"  4*(cw->xSnapSpacing)))" "Move object 4x grid to right")
     
     (daisySetBindKey  "Layout" "Ctrl<Key>m" "(leHiCreateMPP)" "Create a multipart path")
     (daisySetBindKey  "Layout" "AltShift<Key>p" "(let ((cw (hiGetCurrentWindow))) (if (equal \"instance\" cw->instLabel) (cw->instLabel=\"master\") (cw->instLabel=\"instance\")))" "Toggle showing the instance name or master name")
     (daisySetBindKey  "Layout" "AltShift<Key>j" "(daisyLePlot nil)" "Plot the layout window")
     
     (daisySetBindKey  "Layout" "AltShift<Key>n" "(let ((cw (hiGetCurrentWindow))) (cw -> netsOn = (not (cw -> netsOn))))" "Toggle the net display")
     (daisySetBindKey  "Layout" "AltShift<Key>b" "(let ((cw (hiGetCurrentWindow))) (cw -> useTrueBBox = (not (cw -> useTrueBBox))))" "Toggle showing the true bounding box")
     
     (daisySetBindKey  "Layout" "AltShift<Key>m" "(leDisplaySomeLayersOnly nil)" "Display metal and their corresponding lables and pins, only")
     (daisySetBindKey  "Layout" "AltShift<Key>a" "(leDisplaySomeLayersOnly \"all\")" "Display all layers")
     (daisySetBindKey  "Layout" "AltShift<Key>l" "(lePrintLabels)" "Print all labels found in the layout hierarchy")
     (daisySetBindKey  "Layout" "AltShift<Key>t" "(daisyLeObjectInfo)" "Return information about the object")
     (daisySetBindKey  "Layout" "AltShift<Key>x" "(getXY)" "Print the current xy-position of the mouse pointer in the CIW")     
     (daisySetBindKey  "Layout" "Ctrl<Key>d" "(geDeselectAll)" "Deselect everything")
     
     (daisySetBindKey  "Layout" "Alt<Key>f" "(leZoomToSelSet)" "Zoom to selected set of objects")
     (daisySetBindKey  "Layout" "Alt<Key>k" "(daisyLeCreateRulerMarker)" "Create a ruler marker for marking object and finding after zoom-out")
     (daisySetBindKey  "Layout" "Ctrl<Key>k" "(daisyLeCreateHilite)" "Create a  20-um circle ")
     (daisySetBindKey  "Layout" "CtrlShift<Key>k" "geDeleteAllHilightSet(hiGetCurrentWindow()->cellView)" "Remove all highlights.  ")

     (daisySetBindKey  "Layout" "Alt<Key>x" "(leEIPZoomAbsoluteScale)" "Zoom to fit current edit-in-place object")
     
     ;; (daisySetBindKey  "Layout" "AltShift<Key>l" "(daisyLeObjectInfo \"magma\")" )
     
     (daisySetBindKey  "Layout" "Ctrl<Key>l" "(ltf) (hiZoomAbsoluteScale (hiGetCurrentWindow) 0.9)" )
     (daisySetBindKey  "Symbol" "Ctrl<Key>l" "(ltf) (hiZoomAbsoluteScale (hiGetCurrentWindow) 0.9)" )
     ;; Common keys
     (progn 
       (let ((toolList (append  (if (not (equal (getShellEnvVar "CDSDATABASE") "oa")) (list "awv" "Abstract Editor")) 
				(list "Text" "Schematics" "Layout" "Symbol" "Command Interpreter" ))))
	 (foreach tool toolList
	 	  (daisySetBindKey tool "Ctrl<Key>l" "(ltf) (hiZoomAbsoluteScale (hiGetCurrentWindow) 0.9)" )

		  (daisySetBindKey tool "Alt<Key>Home" "(progn (hiDisplayWindow (hiGetCIWindow)) (hiFocusToCIW) (hiRaiseWindow (hiGetCIWindow)))" "Open the CIW")
		  (daisySetBindKey tool "Alt<Key>End" "(ddsOpenLibManager)" "Open the library manager")
		  (daisySetBindKey tool "<Key>F8" "(loadi \"daisy/skill/daisyMultiPaths.il\")" "Reload the layout functions.")
		  (daisySetBindKey tool "<Key>F9" "(loadi \".cdsinit\")" "Reload the whole tjabang.")
		  (daisySetBindKey tool "<Key>F10" "(loadi \"daisy/skill/daisyBindkeys.il\")" "Reload the bindkeys")
		  (daisySetBindKey tool "<Key>F11" "(leRaiseLSW)" "Raise the LSW")   
		  (daisySetBindKey tool "<Key>F12" "(daisyPrintBindKeys)" "Print bindkeys")   
		  (daisySetBindKey tool "Ctrl<Key>W" "(hiCloseWindow (getCurrentWindow))" "Close the window")		  
		  (daisySetBindKey tool "None<Btn3Down>" "" "Changed repeat command")))
       t)
     ))

(if (not (equal (getShellEnvVar "CDSDATABASE") "oa"))
    (progn

      ;; Analog waveform window callbacks
      
      (daisySetBindKey  "awv" "Shift<Key>y" "(awviSwitchAxisModeCB)" "Strip the waveforms")
      (daisySetBindKey  "awv" "Shift<Key>o" "(awviSwitchAxisModeCB)" "Strip the waveforms")
      (daisySetBindKey  "awv" "Shift<Key>s" "(daisyDistributeCurves)" "Distribute the curves over the whole waveform window.")
      (daisySetBindKey  "awv" "Shift<Key>x" "(daisyAwvAxeThem)" "Combine all curves into the same axis")
      (daisySetBindKey  "awv" "Shift<Key>p" "(daisyAwvPlot)" "Plot window to file (eps,tiff,pdf)")
      
      (daisySetBindKey  "awv" "Ctrl<Key>f"  "(awvFatifyCurves)" "Make all curves bold")
      
      (daisySetBindKey  "awv" "<Key>e"  "(printf \"<Delete All> Removed! \\n \")" "Redefined the delete all button") 
      
      (daisySetBindKey  "awv" "Ctrl<Key>g"  "(awvToggleAllGrids)" "Toggle all the grids in the waveform")
      
      (for m 1 9 (daisySetBindKey  "awv" (sprintf nil "Ctrl<Key>%d" m)  (sprintf nil "(awvToggleSubwindowDisplayStatus  %d)" m) (sprintf nil "Turn on/off subwindow %d." m)))
      
      (daisySetBindKey  "awv" "Ctrl<Key>0"  "(awvToggleSubwindowDisplayStatus 10)" "Turn on/off subwindow 10")
      (daisySetBindKey  "awv" "Ctrl<Key>W"  "(hiCloseWindow (getCurrentWindow))" "Close the window")
      ))
