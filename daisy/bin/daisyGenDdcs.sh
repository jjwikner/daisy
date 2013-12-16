#!/bin/tcsh -f

#
# Project setup for the daisy cad environment
#
# This file creates all the required/suggested subdirectories in 
# each ddc.
# 
# J Jacob Wikner, Jacob.Wikner@LiU.se
#

if ($# < 1) then
    # Assume $PROJAREA is set    
else     
    setenv PROJAREA $1
endif

if ( ! ($?PROJAREA) ) then
    echo "PROJAREA not set, you have probably not sourced the project"
else

# Link all the repos

if (-e  $PROJAREA/daisyProjSetup/info/daisyDdcs.txt) then 
    foreach line (`cat $PROJAREA/daisyProjSetup/info/daisyDdcs.txt`)
         echo "Generating the libs for $line"
	 daisyGenDdcSubdirs.sh $line
    end
else
    echo "There is no daisyDdcs.txt file in your daisyProjSetup/info"
endif

endif  
