(printf "DAISY:: ===========> ... setting pdk environment.\n")

;; Grid
(envSetVal "layout" "xSnapSpacing" 'float 0.005)
(envSetVal "layout" "ySnapSpacing" 'float 0.005)

(asiSetEnvOptionVal "spectre" 'includePath 
		    (strcat 
		     "/sw/cadence/libraries/cmos040lp_11a/DK_cmos040lp_AMS_7m4x0y2z_2V51V8@1.1a/DATA/SPECTRE/CORNERS1V8 "
		     (getShellEnvVar "PDK_HOME") "/" "sim/models"))



;; (asiSetEnvOptionVal "spectre" 'includePath "/sw/cadence/libraries/cmos040lp_21/PDK_STM_cmos040lp_AMS_7m4x0y2z_2V51V8@2.1/SIMU/SPECTRE/1V8")

(envSetVal "asimenv.startup" "projectDir" 'string (strcat "/tmp/" (getShellEnvVar "USERAREA") "/sim"))

(envSetVal "spectre.envOpts" "switchViewList" 'string "SimMosfetStandard SimCapaStd SimCapaAcc SimMosfetAccurate SimDiodeStandard SimMosfetrfStandard SimMosfetrfPSP SimInductorStandard SimInductorAccurate SimVaractorStandard SimCmomStandard SimCmomAccurate SimCpolyStandard SimCmetalStandard SimCfringeStandard SimCfringeAccurate SimResistorStandard SimResistorAccurate SimResistorHF SimResistorHFV SimBipolarStandard SimCmimStandard spectre auCmos_sch cmos_sch cmos.sch ads_schematic schematic auGate_sch auGate.sch gate_sch gate.sch extracted ahdl veriloga")


(envSetVal "spectre.envOpts" "stopViewList" 'string "SimMosfetStandard SimCapaStd SimCapaAcc SimMosfetAccurate SimDiodeStandard SimMosfetrfStandard SimMosfetrfPSP SimInductorStandard SimInductorAccurate SimVaractorStandard SimCmomStandard SimCmomAccurate SimCpolyStandard SimCmetalStandard SimCfringeStandard SimCfringeAccurate SimResistorStandard SimResistorAccurate SimResistorHF SimResistorHFV SimBipolarStandard SimCmimStandard spectre ahdl veriloga")
