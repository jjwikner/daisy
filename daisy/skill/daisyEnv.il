;; daisyEnv.il
;; ------------------------------------------------------

(printf "DAISY:: ... loading %s.il ...\n" "daisyEnv")
(setq      WORKAREA (getShellEnvVar "WORKAREA"))

;; Schematic settings
(printf " ... ... setting schematic properties ... \n")
(if (envIsVal "schematic" "dimming") (envSetVal "schematic"  "dimming"  'boolean  t))
(if (envIsVal "schematic" "dimmingMode") (envSetVal "schematic"  "dimmingMode"  'string  "Automatic"))
(if (envIsVal "schematic" "dimmingIntensity") (envSetVal "schematic"  "dimmingIntensity"  'int  50))

;; Layout settings
(printf " ... ... setting layout properties ... \n")
(envSetVal "layout"  "useTrueBBox"      'boolean  t)
(envSetVal "layout"  "instLabel"        'string   "instance")
(envSetVal "layout"  "originMarkersOn"  'boolean  t)
(if (envIsVal "layout" "dimming") (envSetVal "layout"  "dimming"  'boolean  t))
(if (envIsVal "layout" "dimmingMode") (envSetVal "layout"  "dimmingMode"  'string  "Automatic"))
(if (envIsVal "layout" "dimmingIntensity") (envSetVal "layout"  "dimmingIntensity"  'int  50))
(hiSetUndoLimit 8)
;; Layout XL
(if (envIsVal "layoutXL" "templateFileName") (envSetVal "layoutXL" "templateFileName" 'string (strcat (getShellEnvVar "DAISYAREA") "/daisyLayout.lxt")))
(envSetVal "layoutXL" "createBoundaryLabel"    'boolean	t)
(envSetVal "layoutXL.placement" "createPinRow" 'boolean t)
(envSetVal "layoutXL" "lxGroundNetNames" 'string	
	   "gnd gnd! gnd: vss vss! vss: vgnd vgndGuard vgndana vgnddig vsub")
(envSetVal "layoutXL" "lxSupplyNetNames" 'string	
	   "gnd gnd! gnd: vss vss! vss: vpwr vpwrGuard vpwr33 vpwr10 vpwr33ana vpwr33dig vpwr10ana vpwr10dig")
(envSetVal "layoutXL" "lxLocalAbutment" 'boolean t)

;; Simulator settings
(printf "DAISY:: ... ... setting simulator properties ... \n")

(envSetVal "asimenv.misc"     "numberOfSavedRuns"  'int     0)
(envSetVal "asimenv.misc"     "oceanScriptFile"    'string  (strcat (getShellEnvVar "USERAREA") "/sim/ocean/oceanScript.ocn"))

(envSetVal "asimenv.plotting" "artistPlottingMode" 'string  "Append")
(envSetVal "asimenv.plotting" "immediatePlot"      'boolean t)

(if (envIsVal "asimenv.startup"  "cds_ade_wftool") (envSetVal "asimenv.startup"  "cds_ade_wftool"     'string  "awd"))
(envSetVal "asimenv.startup"  "projectDir"         'string  (strcat (getShellEnvVar "USERAREA") "/sim"))
(envSetVal "asimenv.startup"  "remoteDir"          'string  (strcat (getShellEnvVar "USERAREA") "/sim"))

(envSetVal "asimenv.startup"  "host"               'string  (if (getShellEnvVar "REMOTEHOST") (getShellEnvVar "REMOTEHOST") (getShellEnvVar "HOST")))

(envSetVal "asimenv.distributed" "logFile" 'string (strcat (getShellEnvVar "USERAREA")  "/log/cdsDPSetupChk.log"))
(envSetVal "ams.compilerOpts"	"maxErrors"	'string	"150")

(envSetVal "asimenv"          "schematicBased"     'boolean t)
(envSetVal "asimenv"	      "saveAsCellview"     'boolean t)
(envSetVal "asimenv"	      "loadCorners"        'boolean nil)
(envSetVal "asimenv"	      "saveDir"            'string  (strcat (getShellEnvVar "USERAREA")  "/sim/artist_states"))

(if (envIsVal "asimenv" "mailTo") (envSetVal "asimenv" "mailTo" 'string (if (getShellEnvVar "EMAIL") (getShellEnvVar "EMAIL") "")))

(envSetVal "spectre.qpss"     "evenodd"                 'string   "auto")
(envSetVal "spectre.envOpts"  "switchViewList"  'string  "spectre spice cmos_sch cmos.sch schematic extracted veriloga verilogams ams ahdl")
(envSetVal "spectre.envOpts"  "printComments"  'boolean t)

;; Ultrasim
(if (envIsVal "UltraSim.opts" "wf_format") (envSetVal "UltraSim.opts" "wf_format" 'string "PSF"))

;; Waveform Settings
(envSetVal "wavescan.application" "docdirectory"  'string  (strcat (getShellEnvVar "USERAREA") "/log"))
(if (envIsVal "wavescan.application" "envdirectory") (envSetVal "wavescan.application" "envdirectory"  'string  (strcat (getShellEnvVar "USERAREA") "/log")))

(printf " ... ... setting schematic properties ... \n")

(envSetVal "schematic"        "netNameMethod"	        'cyclic	"derived")
(envSetVal "schematic"        "netNameRangeDescend"	'boolean	t)
(envSetVal "schematic"        "netNameSeparator"	'string	       "")

(printf " ... ... setting environment properties ... \n")

;; GUI

(envSetVal "cdsLibManager.log" "addUserID"  'boolean  t)
(envSetVal "cdsLibManager.log" "baseName"   'string   "CDS.libManager")

(envSetVal "graphic"           "drawGridOn" 'boolean  nil)

;; ===

(if (equal (getShellEnvVar "CDSDATABASE") "oa")
    (envSetVal "ui" "enableMenuShortcuts" 'boolean nil))

;; For CDL import

(envSetVal "conn2sch"        "log_file_name"   'string  (strcat (getShellEnvVar "USERAREA")  "./log/conn2sch.log"))
(envSetVal "conn2sch"        "minimize_crossovers"     'boolean t)
(envSetVal "conn2sch"        "import_if_exists"        'int     1)

;; For the CDF parameters

(envSetVal "auCore.misc" "updateCDFtermOrder" 'boolean t)

;; Should be removed
;;(drLoadDrf (strcat 
;;	    (getShellEnvVar "PDK_HOME") 
;;	    "/display.drf") nil)

;; DEF / LEF export/import settings

(setq dfIILEFDEFCurrentVersion "5.5")

;; Verilog In

(setq ihdl_use_van t)

;; Verilog netlisting

(setq hnlVerilogNetlistBehavioralExplicit t)


;; Project specific corners and simulator options

(envSetVal "spectre.opts"      "temp"       'string   "70")
(envSetVal "spectre.opts"      "gmin"       'string   "1e-15")

;; To guarantee that the parametric analysis netlists properly
(envSetVal "spectre.envOpts" "controlMode" 'string "batch")

;; Add project env

(if (not onlyOcean)    
    (if (isFile (strcat (getShellEnvVar "PROJAREA") "daisyProjSetup/skill/daisyEnv.il"))
	(loadi (strcat (getShellEnvVar "PROJAREA") "daisyProjSetup/skill/daisyEnv.il"))))

(setq vlogExpandIteratedInst t)

(if (equal (getShellEnvVar "CDSDATABASE") "oa")
    (envSetVal "license"  "ADELicenseCheckoutOrder" 'string "GXL,XL,L"))

;; ------------------------------------------------------
;; End of daisyEnv.il
