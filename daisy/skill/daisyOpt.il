;; Load an optimization tool
;; Currently the flow is only setup for the Analog Dimensions
;; Optimizer

;; Check if it is set to be used in currently loaded project

(if (getShellEnvVar "DAISY_USES_ANADIM")
    (progn
      
      (loadi (strcat (getShellEnvVar "ANALOG_DIMENSIONS_HOME")
		     "/skill/"
		     "AnalogDimensionsInit.ile"))
))

		     
