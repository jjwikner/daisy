#!/bin/sh

#
# A small stream-in snippet for the cadence 5 environment.
# To be updated for the cadence 6 environment.
#

if test $# -lt 2 
then
    echo "No no no no!"
else

    STREAMFILE=$1
    STREAMLIB=$2
    

# ============

if test $# -gt 2
    then
    STREAMDDC=$3
    STREAMPWD=$PROJAREA/$STREAMDDC/tapeout/
    LOGPATH=$STREAMPWD/../log/
    else
    STREAMDDC=""
    STREAMPWD=$PWD
    LOGPATH=$USERAREA/log/
fi

# ============

STREAMTMP=$LOGPATH/streamIn.pipo.$STREAMLIB.pipo.settings
STREAMERR=$LOGPATH/streamIn.pipo.$STREAMLIB.err.log

# ============

TECHLIB=""
STREAMMAP=$PDK_HOME/pdk/streamLayers.map
REFLIBS=`cat $PDK_HOME/pdk/streamRefLibs.txt`

# ============

if [ -w "$STREAMTMP" ]
    then
    rm -f  $STREAMTMP
fi

echo "Creating stream-in template for $STREAMLIB ..."

echo "(setq streamFile   \"$STREAMFILE\")"        >> $STREAMTMP 
echo "(setq streamLib    \"$STREAMLIB\")"         >> $STREAMTMP 
echo "(setq streamTechLib      \"$TECHLIB\")"           >> $STREAMTMP 
echo "(setq streamRefLibs      \"$REFLIBS\")"           >> $STREAMTMP 
echo "(setq streamRunDir \"$STREAMPWD\")"               >> $STREAMTMP 
echo "(setq streamErrFile \"$STREAMERR\")"    >> $STREAMTMP 
echo "(setq streamMap \"$STREAMMAP\")"    >> $STREAMTMP 

cat $DAISYAREA/import/streamIn.tmpl          >> $STREAMTMP 

echo "Starting stream-in process ..."

cd $WORKAREA
echo "Stream log file is $LOGPATH/streamIn.$STREAMLIB.pipo.log"
pipo strmin $STREAMTMP > $LOGPATH/streamIn.$STREAMLIB.pipo.log


  
fi
