#!/bin/sh

# We need to have a flag to check document type too.

DDCNAME=$1 
DOCUMENT=$3
LIBNAME=$2     

DOCNAME=`basename $DOCUMENT`
DOCPATH=`find $DOCUMENT -printf "%h"`
DOCLIB=`echo $DOCNAME | sed 's/\./_/g'`

if [ -e $PWD/$DOCNAME ]
then
    DOCPATH=$PWD    
else    
    DOCPATH=$DOCPATH
fi
mkdir -p $WORKAREA/$DDCNAME/cds/$LIBNAME/$DOCLIB/document


ln   -sf $DOCPATH/$DOCNAME $WORKAREA/$DDCNAME/cds/$LIBNAME/$DOCLIB/document/document.pdf
# This is from the data.reg file in the cadence domain.
echo "document.pdf" > $WORKAREA/$DDCNAME/cds/$LIBNAME/$DOCLIB/document/master.tag

