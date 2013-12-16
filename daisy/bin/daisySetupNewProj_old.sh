#!/bin/csh -f
#
# Project setup for the daisy cad environment
# J Jacob Wikner 
#

# Path to your project area
setenv PROJPATH $1
# Project name
setenv PROJNAME $2
setenv PROJAREA $PROJPATH/$PROJNAME
mkdir -p $PROJAREA
setenv DAISYAREA /site/edu/es/DAISY/daisy
cp -r $DAISYAREA/../daisyProjSetup.tmpl $PROJAREA/daisyProjSetup

echo "setenv PROJAREA $PROJAREA" >>  $PROJAREA/daisyProjSetup/cshrc/tcshrc

echo "Don't forget to: "
echo " 1) change in the tcshrc file (group and process) "
echo " 2) update the daisyDdcs.txt file in daisyProjSetup/info "
echo " 3) rerun the daisyGenDdcs.sh to update all ddc subdirectories "

cd $HOME
$DAISYAREA/bin/daisySetupProj.sh $HOME $PROJNAME $PROJAREA
cd $HOME
source .$PROJNAME_rc
$DAISYAREA/bin/daisyGenDdcs.sh 

# etc.


