(printf "DAISY:: ...  loading libInit to set up sim models ...\n")

(setq libPath 
      (strcat
      (getShellEnvVar "DAISYAREA")
      "/pdkSpecific/"
      (getShellEnvVar "CDSPROCESSNAME")
      "/sim/corners.scs"))

(setq modelName "pmos")

(setq libPathLocal
      (strcat 
       (getShellEnvVar "DAISYAREA") 
       "/pdkSpecific/"
       (getShellEnvVar "CDSPROCESSNAME")
       "/sim/models/"))

;;   setup spectre model files, sections

(if (isContextLoaded "schView") then
  (envSetVal "asimenv.startup" "simulator" 'string "spectre")
  (asiSetEnvOptionVal 
   (asiGetTool 'spectre) "modelFiles"
   (setq modelList 
	 (list 
	  (list (strcat libPathLocal "local.scs") "$daisyCorner")))))
  
