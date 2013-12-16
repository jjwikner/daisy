
(printf " ... setting layout properties ... \n")
;; Grid
(envSetVal "layout" "xSnapSpacing" 'float 0.005)
(envSetVal "layout" "ySnapSpacing" 'float 0.005)

(envSetVal "asimenv.startup" "projectDir" 'string (strcat "/tmp/" (getShellEnvVar "USERAREA") "/sim"))
