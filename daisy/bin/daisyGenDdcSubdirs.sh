#!/bin/bash 

#
# Project setup for the daisy cad environment
#
# This file creates all the required/suggested 
# subdirectories in one specified ddc. The project
# must have been sourced.

# 
# J Jacob Wikner, Jacob.Wikner@LiU.se
#

echo "DAISY::$0"

if [ $# -lt 1 ] 
then
    echo "DAISY:: You need to specify a DDC!"
else     
    DDCNAME=$1    
    if [ -e "$PROJAREA" ] 
    then
	if [ $# -eq 2 ] 
	then # Assuming second argument implies "local" or so, 
	     # rather than writing in the project area
	    TRGTAREA="$WORKAREA"
	else
	    TRGTAREA="$PROJAREA"
	fi

    # --------------------------------
    # This could be done a bit neater.
    # Maybe we should pick from a list of libraries
    # in the daisy area, rather than hardcoding it here.
    # --------------------------------
		
	echo "DAISY:: Generating the libs for $DDCNAME"
	
	mkdir -p "$TRGTAREA/$DDCNAME/doc/figs"   # any figures
	mkdir -p "$TRGTAREA/$DDCNAME/doc/mfigs"  # matlab figures 
	mkdir -p "$TRGTAREA/$DDCNAME/bin"        # binaries
	mkdir -p "$TRGTAREA/$DDCNAME/log"        # log files
	mkdir -p "$TRGTAREA/$DDCNAME/info"       # information, README, etc.
	mkdir -p "$TRGTAREA/$DDCNAME/cshrc"      # shell startup scripts
	mkdir -p "$TRGTAREA/$DDCNAME/oa"         # cadence oa format
	mkdir -p "$TRGTAREA/$DDCNAME/cds"        # cadence cdba format
	mkdir -p "$TRGTAREA/$DDCNAME/sim/ocean"  # simulation directories
	mkdir -p "$TRGTAREA/$DDCNAME/skill"      # skill scripts
	mkdir -p "$TRGTAREA/$DDCNAME/tcl"        # tcl scripts
	mkdir -p "$TRGTAREA/$DDCNAME/rtl/mgc"    # rtl and mentor area
	mkdir -p "$TRGTAREA/$DDCNAME/asm"        # Assembler (and uC code)
	mkdir -p "$TRGTAREA/$DDCNAME/pv/lvs"     # Physical verification, lvs
	mkdir -p "$TRGTAREA/$DDCNAME/pv/ant"     # Antenna
	mkdir -p "$TRGTAREA/$DDCNAME/pv/drc"     # Rule checker
	mkdir -p "$TRGTAREA/$DDCNAME/pv/xrc"     # RC extract
	mkdir -p "$TRGTAREA/$DDCNAME/pv/gds"     # Intermediate GDS
	mkdir -p "$TRGTAREA/$DDCNAME/pv/cdl"     # Intermediate netlists
	mkdir -p "$TRGTAREA/$DDCNAME/pv/log"     # Log files
	mkdir -p "$TRGTAREA/$DDCNAME/m/"         # MATLAB
	mkdir -p "$TRGTAREA/$DDCNAME/tapeout"    # Release directory
	mkdir -p "$TRGTAREA/$DDCNAME/sch"        # Schematics (PCB, etc.)
	mkdir -p "$TRGTAREA/$DDCNAME/py"         # Python scripts
	mkdir -p "$TRGTAREA/$DDCNAME/je"         # Electric database
	mkdir -p "$TRGTAREA/$DDCNAME/mo"         # Don't remember ...
	mkdir -p "$TRGTAREA/$DDCNAME/pnr"        # Place-and-route
	mkdir -p "$TRGTAREA/$DDCNAME/syn"        # Synthesis

	# --------------------------------------------------------
	# This list of default files could be further extended.
	# --------------------------------------------------------

	echo "DAISY:: Updating some start-up files, m/skill/cshrc"
	
	echo "%% $DDCNAME" >> "$TRGTAREA/$DDCNAME/m/daisy.m"
	echo ";; $DDCNAME" >> "$TRGTAREA/$DDCNAME/skill/daisy.il"
	echo "## $DDCNAME" >> "$TRGTAREA/$DDCNAME/cshrc/tcshrc"
	echo "## $DDCNAME" >> "$TRGTAREA/$DDCNAME/info/README"
	echo "====="
	
    else
    
	echo "DAISY:: Environment variable PROJAREA not set! "
        echo "DAISY:: You have probably not sourced the project."
    fi
fi
