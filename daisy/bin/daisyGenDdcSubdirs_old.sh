#!/bin/csh -f

#
# Project setup for the daisy cad environment
#
# This file creates all the required/suggested 
# subdirectories in one specified ddc. The project
# must have been sourced.

# 
# J Jacob Wikner, Jacob.Wikner@LiU.se
#
    echo "here?"
if ($# < 1) then
    echo "DAISY:: You need to specify a DDC!"
endif
    echo "here?"

if ($# < 1) then
    echo "DAISY:: You need to specify a DDC!"
else     
    
    setenv DDCNAME $1

    if ( ! ($?PROJAREA) ) then
	echo "DAISY:: PROJAREA not set! "
        echo "DAISY:: You have probably not sourced the project."
    else
    
    # --------------------------------
    # This could be done a bit neater.
    # --------------------------------

    if ($# == 2) then
       setenv TRGTAREA "$WORKAREA"
     else
       setenv TRGTAREA "$PROJAREA"
      endif
        
        echo "DAISY:: Generating the libs for $DDCNAME"

         mkdir -p "$TRGTAREA/$DDCNAME/doc/figs"
         mkdir -p "$TRGTAREA/$DDCNAME/doc/mfigs"
         mkdir -p "$TRGTAREA/$DDCNAME/bin"
         mkdir -p "$TRGTAREA/$DDCNAME/log"
         mkdir -p "$TRGTAREA/$DDCNAME/info"
         mkdir -p "$TRGTAREA/$DDCNAME/cshrc"
         mkdir -p "$TRGTAREA/$DDCNAME/oa"
         mkdir -p "$TRGTAREA/$DDCNAME/cds"
         mkdir -p "$TRGTAREA/$DDCNAME/sim/ocean"
         mkdir -p "$TRGTAREA/$DDCNAME/skill"
         mkdir -p "$TRGTAREA/$DDCNAME/rtl"
         mkdir -p "$TRGTAREA/$DDCNAME/pv/lvs"
         mkdir -p "$TRGTAREA/$DDCNAME/pv/drc"
         mkdir -p "$TRGTAREA/$DDCNAME/pv/xrc"
         mkdir -p "$TRGTAREA/$DDCNAME/pv/gds"
         mkdir -p "$TRGTAREA/$DDCNAME/pv/cdl"
         mkdir -p "$TRGTAREA/$DDCNAME/pv/log"
         mkdir -p "$TRGTAREA/$DDCNAME/m/"
	 mkdir -p "$TRGTAREA/$DDCNAME/tapeout"
	 mkdir -p "$TRGTAREA/$DDCNAME/sch"
	 mkdir -p "$TRGTAREA/$DDCNAME/py"
	 mkdir -p "$TRGTAREA/$DDCNAME/je"

	 echo "DAISY:: Updating some start-up files, m/skill/cshrc"

	 echo "%% $DDCNAME" >> "$TRGTAREA/$DDCNAME/m/daisy.m"
	 echo ";; $DDCNAME" >> "$TRGTAREA/$DDCNAME/skill/daisy.il"
	 echo "## $DDCNAME" >> "$TRGTAREA/$DDCNAME/cshrc/tcshrc"
	 echo "====="

	 endif
endif
