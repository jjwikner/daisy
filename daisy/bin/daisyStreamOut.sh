#!/bin/sh

#
# A small stream-out snippet for the cadence 5 environment.
# To be updated for the cadence 6 environment.
#

# Input arguments:

if test $# -eq 3
then
echo "No no no no!"
else

STREAMLIB=$1
STREAMCELL=$2
STREAMFILE=$3

# ============

LOGPATH=$USERAREA/log/
STREAMERR=$LOGPATH/streamOut.pipo.$STREAMLIB.$STREAMCELL.err.log

# ============
if test $# -eq 4
then
DDC=$4
STREAMSKL="$PROJAREA/$DDC/skill/streamOut.il"
STREAMSKL=""
STREAMPWD="$PROJAREA/$DDC/tapeout"
STREAMTMP=$STREAMPWD/../log/streamOut.pipo.$STREAMLIB.$STREAMCELL.settings
else
DDC=""
STREAMSKL=""
STREAMPWD=$PWD
STREAMTMP=$LOGPATH/streamOut.pipo.settings
fi

# ============
STREAMMAP=$PDK_HOME/pdk/streamLayers.map
STREAMREF=`cat $PDK_HOME/pdk/streamRefLibs.txt`
# ============

if [ -w "$STREAMTMP" ]
    then
    rm -f  $STREAMTMP
fi

echo "Creating stream-out template for $STREAMCELL ..."

echo "(setq streamFile    \"$STREAMFILE\")"        >> $STREAMTMP 
echo "(setq streamLib     \"$STREAMLIB\")"         >> $STREAMTMP 
echo "(setq streamCell    \"$STREAMCELL\")"        >> $STREAMTMP 
echo "(setq streamLayers  \"$STREAMMAP\")"         >> $STREAMTMP 
echo "(setq streamRunDir  \"$STREAMPWD\")"         >> $STREAMTMP 
echo "(setq streamErrFile \"$STREAMERR\")"     >> $STREAMTMP 
echo "(setq streamRefLibs \"$STREAMREF\")"         >> $STREAMTMP 
echo "(setq streamSkill   \"$STREAMSKL\")"         >> $STREAMTMP 

cat $WORKAREA/daisy/export/streamOut.template      >> $STREAMTMP 

echo "Starting stream-out process ..."

cd $WORKAREA
echo "Stream log file is $LOGPATH/streamOut.$STREAMCELL.pipo.log"

# This command is replaced in OA (cadence 6)
pipo strmout $STREAMTMP > $LOGPATH/streamOut.$STREAMCELL.pipo.log

  
fi
