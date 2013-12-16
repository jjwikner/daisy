(printf " ...  loading libInit to set up sim models ...\n")

(setq libPath 
      (strcat (getShellEnvVar "CDS_HOME") 
	      "/tools/dfII/samples/artist/mixSig/models/spectre/"))

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
	  (list (strcat  libPath "cds_msdev" ".scs") "")
	  (list (strcat libPathLocal "local.scs"))
	  ;;(list (strcat  libPath "nmos" ".scs") "")
	  ))))
  
