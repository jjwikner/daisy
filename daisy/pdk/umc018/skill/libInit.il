(printf " ...  loading libInit to set up sim models ...\n")

(setq libPath 
      (strcat (getShellEnvVar "CDS_HOME") 
	      "/tools/dfII/samples/artist/mixSig/models/spectre/"))

(setq modelName "pmos")

;;   setup spectre model files, sections

(if (isContextLoaded "schView") then
  (envSetVal "asimenv.startup" "simulator" 'string "spectre")
  (asiSetEnvOptionVal 
   (asiGetTool 'spectre) "modelFiles"
   (setq modelList 
	 (list 
	  (list (strcat  libPath "pmos" ".scs") "")
	  (list (strcat  libPath "nmos" ".scs") "")))))
  
