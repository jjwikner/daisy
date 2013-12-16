#!/bin/csh -f
#
# This is to be used for courses at ISY
#
# J Jacob Wikner (jacob.wikner@liu.se)
#

## 2011-05-27: Cleaned up a bit, removed the workarea 
## links and relied instead on the Projarea variable. 
## Further on, cleaned up the cds.lib generation to 
## point to daisyarea rather than workarea. //JJW

# Local project name
setenv PROJNAME $1        ## For example ANTIK

# Path to your local directory
setenv PROJPATH $HOME     ## Always saved in home directory

# Shared project area
setenv PROJAREA /site/edu/es/$PROJNAME

setenv WORKAREA $PROJPATH/$PROJNAME
setenv DAISYAREA /site/edu/es/DAISY/daisy

mkdir -p $WORKAREA
cd $WORKAREA

# Linking the ProjSetup area
ln -sf   $PROJAREA/daisyProjSetup
ln -sf   $DAISYAREA

setenv USERAREA $WORKAREA/work_$USER 
mkdir -p $USERAREA

# Creating the personal work directory structure (local)
$DAISYAREA/bin/daisyGenDdcSubdirs.sh "work_$USER" local
# Create local cds.lib so that Cadence don't complain that they don't exist
touch "$USERAREA/cds/cds.lib"
touch "$USERAREA/cds/cds.testlib"
touch "$USERAREA/oa/cds.lib"
touch "$USERAREA/oa/cds.testlib"


# Linking the cadence setup files and dot files
ln -sf $DAISYAREA/cds/cdsinit     .cdsinit
ln -sf $DAISYAREA/cds/cdsenv      .cdsenv
ln -sf $DAISYAREA/cds/cdsplotinit .cdsplotinit
ln -sf $DAISYAREA/oceanrc         .oceanrc
ln -sf $DAISYAREA/data.reg 
ln -sf $DAISYAREA/hdl.var

# Setting up simulation directories for ocean
ln -sf $DAISYAREA/oceanrc         $USERAREA/sim/ocean/.oceanrc
ln -sf $DAISYAREA/cds/cdsinit     $USERAREA/sim/ocean/.cdsinit
ln -sf $DAISYAREA/cds/cdsplotinit $USERAREA/sim/ocean/.cdsplotinit


# Creating the cds.lib file
echo 'INCLUDE $DAISYAREA/cds/$CDSDATABASE.lib' >! cds.lib

# Write to the rc file in the $HOME to be sourced
echo "# Setup for $PROJNAME" >! $HOME/.${PROJNAME}_rc
echo "setenv PROJNAME $PROJNAME" >> $HOME/.${PROJNAME}_rc
echo "setenv WORKAREA $WORKAREA" >> $HOME/.${PROJNAME}_rc
echo "setenv DAISYAREA $DAISYAREA" >> $HOME/.${PROJNAME}_rc
echo 'source $DAISYAREA/cshrc/tcshrc' >> $HOME/.${PROJNAME}_rc

# Link all the repos
if (-e  $PROJAREA/daisyProjSetup/info/daisyDdcs.txt) then 
    foreach line (`cat $PROJAREA/daisyProjSetup/info/daisyDdcs.txt`)
         echo "Adding $line to your $WORKAREA"
	 ln -sf $PROJAREA/$line
    end
else
    echo "DAISY:: There is no daisyDdcs.txt file in your daisyProjSetup"
endif

# Run project specific setup

if (-e $PROJAREA/daisyProjSetup/bin/daisyProjSetupAdd.sh) then
    $PROJAREA/daisyProjSetup/bin/daisyProjSetupAdd.sh
endif

## End of files


