(daisyLog " ... setting layout properties ... \n")
;; Grid
(envSetVal "layout" "xSnapSpacing" 'float 0.025)
(envSetVal "layout" "ySnapSpacing" 'float 0.025)

;; Set up model files for APS simulator to the same as Spectre
(daisyLog " ... setting APS simulator model files ... \n")
(asiSetEnvOptionVal (asiGetTool 'aps) "modelFiles" (asiGetEnvOptionVal (asiGetTool 'spectre) "modelFiles"))

