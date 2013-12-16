#!/bin/tcsh
#

# Built on Kent Palmqvist's work in the TSTE12 course packages.

echo "DAISY::$0"

printf "Starting the daisyMentor.sh (mentorskal) \n"
# /usr/bin/xterm -g 100x50+65+0 -ls -sb -title "Mentor shell enviroment" -e /bin/tcsh 

printf "Done with daisyMentor.sh\n"

# printf  "\nWait...";setenv HDS_LIBS /proj/tde/labs/labgrupp$grupp/hds.hdp ; setenv HDS_NEW_PROJECT_DIR /proj/tde/labs/labgrupp$grupp/ ; module add mentor xilinx altera ; hdldesigner;

setenv HDS_NEW_PROJECT_DIR $USERAREA/hds
if (! -f  $HDS_NEW_PROJECT_DIR/hds.hdp) then
    cp $HDS_HOME/examples/examples.hdp $HDS_NEW_PROJECT_DIR/hds.hdp
endif
setenv HDS_LIBS $HDS_NEW_PROJECT_DIR/hds.hdp
# module add mentor/precision2010a mentor/modeltech10.0 mentor/Hds2010.2a altera/11.1 ; 
hdl_designer;


exit


# EOF
