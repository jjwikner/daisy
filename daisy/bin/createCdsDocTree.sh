#!/bin/sh

DDCNAME=$1 
LIBNAME=$2 
DOCUMENTS=$3

if [ -z "$DOCUMENTS" ]
  then
    exit;
else
    while read LINE
	do
	    # We need to have a flag to check document type too.
	    createCdsDocLink.sh $DDCNAME $LIBNAME $LINE
    done < $DOCUMENTS
fi;
