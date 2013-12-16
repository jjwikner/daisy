#!/bin/sh

CELLNAME=$1
IPDIR=$2 

TIMESTAMP=`date +"%y%m%d.%H%M"`


LVSEXTFILE=$CELLNAME.extract.sp
LVSLOGFILE=$CELLNAME.lvs.$TIMESTAMP.log
CMDRUNFILE=$CELLNAME.calibre.lvs.run
LVSSRCFILE=$IPDIR/cdl/$CELLNAME.cdl
HCELLSFILE=$IPDIR/pv/$CELLNAME/inc/$CELLNAME.hcell

GDSNAME=$IPDIR/gds/$CELLNAME.gds2.gz
#GDSNAME=$WORKAREA/log/$CELLNAME.gds2.gz

# Create the header of the run file  
echo "// NEW CALIBRE SETTINGS : HEADER"                  > $CMDRUNFILE
echo "LAYOUT PRIMARY \"$CELLNAME\""                       >> $CMDRUNFILE
echo "LAYOUT PATH    \"$GDSNAME\""                        >> $CMDRUNFILE
echo "LAYOUT SYSTEM  GDSII"                               >> $CMDRUNFILE
echo "SOURCE PRIMARY \"$CELLNAME\""                       >> $CMDRUNFILE
echo "SOURCE PATH    \"$LVSSRCFILE\""                     >> $CMDRUNFILE
echo "LVS REPORT     \"$CELLNAME.lvs.$TIMESTAMP.report\"" >> $CMDRUNFILE
echo "LVS IGNORE PORTS NO"                                >> $CMDRUNFILE    
echo "// END OF HEADER"                                   >> $CMDRUNFILE

# Merge local data with master template    
cat $WORKAREA/daisy/pdkSpecific/$CDSPROCESSNAME/verification/calibre.batch.lvs >> $CMDRUNFILE

calibre -lvs -64 -hier -spice $LVSEXTFILE -hcell $HCELLSFILE $CMDRUNFILE  > $LVSLOGFILE 
