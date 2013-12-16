#!/bin/sh

#echo $1
#echo $2

RUNDIR=$1
RULESFILE=$2
HCELLFILE=$3

# Create the local stuff.
RULESDIR=$RUNDIR/../rul/
INCDIR=$RUNDIR/../inc/

# Create the rules directory in the verification area.
# Copy the current rules file.
mkdir -p $RULESDIR
cp $RULESFILE $RULESDIR

# Copy the hcell files.
mkdir -p $INCDIR
ln -sf $PDK_HOME/pv/daisy.hcell $INCDIR/daisy.hcell
cp $HCELLFILE $INCDIR

