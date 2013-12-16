#!/bin/tcsh

# This is for cadence compatibility

setenv CDSBIT "`uname -m | sed 's/^i.*/32b/g' |  sed 's/^x.*/64b/g'`"
echo $CDSBIT

