
(printf " ... setting layout properties ... \n")
;; Grid
(envSetVal "layout" "xSnapSpacing" 'float 0.01)
(envSetVal "layout" "ySnapSpacing" 'float 0.01)

(asiSetEnvOptionVal 
 "spectre" 
 'includePath 
 "/sw/mentor/libraries/cmos065RF_534_IC615.010/DK_cmos065lpgp_RF_7m4x0y2z_2V51V8@5.3.4/DATA/SPECTRE/CORNERS")

;;  /sw/mentor/libraries/cmos065RF_v.522/DK_cmos065lpgp_RF_7m4x0y2z_2V51V8_5.2.2/DATA/SPECTRE/CORNERS/")

(envSetVal "asimenv.startup" "projectDir" 'string (strcat "/tmp/" (getShellEnvVar "USERAREA") "/sim"))
