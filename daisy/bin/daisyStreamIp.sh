#!/bin/sh

#
# A small stream-out/in snippet for the cadence 5 environment.
# To be updated for the cadence 6 environment.
#


if test $# -lt 3
then
    echo "No no no no!"

else

STREAMLIB=$1
STREAMCELL=$2
STREAMDDC=$3

if test $# -lt 4
then
    STREAMTAG=`date +%y%m%d%H%M`
else
    STREAMTAG=$4
fi


STREAMFILE="${STREAMDDC}.${STREAMLIB}.${STREAMCELL}.${STREAMTAG}.gds"
STREAMILIB="${STREAMLIB}_${STREAMTAG}"

echo $STREAMFILE
echo $STREAMILIB

daisyStreamOut.sh $STREAMLIB $STREAMCELL $STREAMFILE $STREAMDDC
daisyStreamIn.sh $STREAMFILE.gz $STREAMILIB $STREAMDDC
cd $WORKAREA
mv $STREAMILIB $PROJAREA/$STREAMDDC/$CDSDATABASE/
echo "UNDEFINE $STREAMILIB" >> cds.lib
cd $PROJAREA/$STREAMDDC/$CDSDATABASE/
daisyAddToCdsLib.sh $STREAMILIB

echo "Remove the DEFINE $STREAMILIB line in your $WORKAREA/cds.lib!"

fi
