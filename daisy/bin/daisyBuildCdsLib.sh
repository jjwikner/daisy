#!/bin/tcsh -f
#
# Project setup for the daisy cad environment
# J Jacob Wikner 
#

if ($# < 1) then
    set cdsName="cds.lib"
else
    set cdsName="cds.testlib"
endif

cd $PROJAREA/daisyProjSetup/$CDSDATABASE/
mv $cdsName "${cdsName}_old"

# Link all the repos
if (-e  $PROJAREA/daisyProjSetup/info/daisyDdcs.txt) then 
    foreach line (`cat $PROJAREA/daisyProjSetup/info/daisyDdcs.txt`)
         echo "Adding $line link to your CDBA/OA lib definitions"
	 echo 'SOFTINCLUDE $PROJAREA/'$line'/$CDSDATABASE/'$cdsName >> $cdsName
    end
else
    echo "There is no daisyDdcs.txt file in your daisyProjSetup"
endif

