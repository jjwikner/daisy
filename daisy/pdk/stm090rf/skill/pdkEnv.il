
(printf " ... setting layout properties ... \n")
;; Grid
(envSetVal "layout" "xSnapSpacing" 'float 0.01)
(envSetVal "layout" "ySnapSpacing" 'float 0.01)


(envSetVal "asimenv.startup" "projectDir" 'string (strcat "/tmp/" (getShellEnvVar "USERAREA") "/sim"))
