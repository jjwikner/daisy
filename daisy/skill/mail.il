(printf " ... loading %s.il ...\n" "mail")

(defun  daisyMail (addr subj msg)
  (let ((mailcmd ""))
    (progn 
      (setq oport (outfile (setq fileName (makeTempFileName (strcat "/tmp/mail" (getShellEnvVar "USER"))))))
      (if oport
	  then
	(fprintf oport subj)
	(fprintf oport  "\r\n=============================================================>\r\n")  
	(fprintf oport msg)
	(fprintf oport  "\r\n<=============================================================\r\n")  
	(close oport))
      (setq mailcmd 
	    (strcat "sendmail -Fdaisy " addr " < " fileName))
      (printf "%s \n" mailcmd)
      (csh mailcmd)
      (deleteFile fileName) )) )

(defun daisySimMail (subj msg)
  (let ((mailcmd ""))
    (progn 
      (setq oport (outfile (setq fileName (makeTempFileName (strcat "/tmp/mail" (getShellEnvVar "USER"))))))
      (if oport
	  then
	(fprintf oport "Subject: %s\r\n" subj)
	(fprintf oport "To: %s\r\n" (getShellEnvVar "EMAIL") )
	(fprintf oport "Simulation Notification")
	(fprintf oport "\r\n=======================\r\n")  
	(fprintf oport msg)
	(close oport))
      (setq mailcmd 
	    (strcat "sendmail -B8BITMIME " 
		    " -Fsimulator@" (getShellEnvVar "HOST") 
		    " -fsimulator@" (getShellEnvVar "HOST") 
		    " " (getShellEnvVar "EMAIL") 
		    " < " fileName " &"))

      (printf "%s \n" mailcmd)
      (csh mailcmd)
      (deleteFile fileName) )))
