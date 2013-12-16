#!/bin/tcsh -f
#

# This simple script lists all the DDCs in the projarea. 
# It is of course assumed that no one has messed up the area.
#

cd $PROJAREA ; find . -maxdepth 1 -mindepth 1 -type d | tr -t '.' ' ' | tr -t '/' ' ' | tr -d ' ' | grep -v daisyProjSetup 

