#!/bin/ksh -p
# This is: gateToOpus.ksh

# invoked from Opus with hiSkillProcess.
# sends on descriptor 3 commands to Opus.

# This process will be interrupted using signal 16 (SIGUSR1)
# to indicate that the data sent to opus is in file $HOME/.dataForOpus
SLEEPTIME=1000
cmd=$0

function msg {
  print "GateToOpus: $*"
}

pid=$$
msg Started process, pid = $pid, ppid = $PPID

/bin/rm -f $HOME/.gateToOpus*
print $$ > $HOME/.gateToOpus.pid
print "REMSH=''; if test `hostname` != \`hostname\`; then REMSH='remsh `hostname`'; fi; \$REMSH /bin/kill -16 $pid" > $HOME/.gateToOpus.wakeMeUp
/bin/chmod 755 $HOME/.gateToOpus.wakeMeUp

dataForOpus=$HOME/.dataForOpus

function sendSkill {
    print >&3 "(debugQuit)"
    print >&3 "$*"
}

function trapInt {
    #msg trapInt...
    trap "kill -CONT $$ ; trap trapInt 16" EXIT 
    sendSkill "(load \"$dataForOpus\")"
    #sendSkill "(printf \"Loaded file %s\\n\" \"$dataForOpus\")"
}

function dieIfOrphaned {
  # parent process $PPID is a sh -c ....
  #ps -lfp $PPID

  # Parent of that is either a serv process or init (pid = 1):
  PPPID=`ps -lfp $PPID | awk 'NR == 2 {print $5}'`
  #ps -lfp $PPPID

  if [[ $PPID -eq 1 || $PPPID -eq 1 ]]
  then
    print "Orphaned. Dying."
    exit 0
  fi
}

function cleanup {
  if [[ ${cleanedUp-0} -eq 0 ]] 
  then
    kill -9 sleepPid
    cleanedUp=1
  fi
}

trap cleanup EXIT QUIT KILL HUP ABRT TERM 

sleep $SLEEPTIME &
sleepPid=$!

while true
do
    if ps -p $sleepPid >/dev/null
    then
      :
    else
      dieIfOrphaned
      sleep $SLEEPTIME &
      sleepPid=$!
    fi
    trap trapInt 16
    msg Zzzzzzzzzz...
    wait $sleepPid
done

