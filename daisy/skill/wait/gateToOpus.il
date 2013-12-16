; Load this file inside Opus.

; A gateway to Opus.
; Using a shell sub process started with hiBeginProcess Unix programs
; can send Skill commands to Opus to execute.
; Uses gateToOpus or gateToOpus.ksh as an inferior process.


GateToOpusPath = "~/skill/"

(defun GateToOpusIOHandler (childId data)
  (printf "%s" data) )

(defun GateToOpusErrHandler (childId data)
  (printf "%s" data) )

(let (cid)
  (if (isFile "/bin/ksh" )
      (setq file "/gateToOpus.ksh")
    (setq file "/gateToOpus"))
  (setq file (strcat GateToOpusPath file ))
  ;; cid = hiSkillProcess( strcat( getShellEnvVar("HOME") file )
  (setq cid (hiSkillProcess file
			    ""
			    "GateToOpusIOHandler"
			    "GateToOpusErrHandler"
			    ""
			    ""))
  (if (and cid (nequal cid 0))
      (info "Started Skill process, child id = %d\n" cid)
    (warn "Could not start gateToOpus properly.\n")))
