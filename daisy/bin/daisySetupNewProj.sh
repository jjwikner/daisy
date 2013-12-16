#!/bin/tcsh -f 
#
# Project setup for the daisy cad environment
# J Jacob Wikner
# Daniel Svärd

# Check for the correct number of arguments before we start to do anything.
if ($#argv != 4) then
  printf "Usage: %s PROJPATH PROJNAME GROUPNAME PROCESSNAME\n" `basename $0`
  printf "Create a new daisy project directory structure for the given process name accessible to the given UNIX group.\n\n"
  printf "Mandatory arguments:\n"
  printf "  PROJPATH\t\tDirectory where the project directory will be created,\n\t\t\t  ie. the parent directory to the project directory.\n"
  printf "  PROJNAME\t\tName of the project. This will also be the name \n\t\t\t  of the project directory inside PROJPATH.\n" 
  printf "  GROUPNAME\t\tThis is the UNIX group name that will have full\n\t\t\t  access to the project.\n"
  printf "  PROCESSNAME\t\tThe name of the process node to be used for this\n\t\t\t  project. A list of supported processes by daisy can\n\t\t\t  be found as subdirectories to the 'pdkSpecific'\n\t\t\t  directory in the daisy area.\n"
  exit
endif

setenv DAISYAREA /site/edu/es/DAISY/daisy
setenv PROJPATH $1
setenv PROJNAME $2
setenv GROUP $3
setenv PROCESS $4

setenv PROJAREA ${PROJPATH}/${PROJNAME}
set setuparea = ${PROJAREA}/daisyProjSetup
setenv PDK_HOME ${DAISYAREA}/pdkSpecific/${PROCESS}

# Create the project area
umask 007
mkdir -p ${PROJAREA}
cp -r ${HOME}/cadence/daisyProjSetup.tmpl ${setuparea}

# Fix the project settings
: >! ${setuparea}/cshrc/tcshrc # Truncate tcshrc file
echo "setenv CDSPROCESSNAME ${PROCESS}" >> ${setuparea}/cshrc/tcshrc
echo "setenv GROUP ${GROUP}" >> ${setuparea}/cshrc/tcshrc
echo "setenv PROJAREA ${PROJAREA}" >> ${setuparea}/cshrc/tcshrc

# Copy in the pdk specific daisyProjSetupAdd.sh
if ( -e ${PDK_HOME}/bin/daisyProjSetupAdd.sh ) then
  cp ${PDK_HOME}/bin/daisyProjSetupAdd.sh ${setuparea}/bin/
endif

# Fix the project tree permissions
${DAISYAREA}/bin/daisyOpen >& /dev/null

echo "Project ${PROJNAME} is setup in ${PROJAREA}."
echo "Don't forget to:"
echo " 1) update the daisyDdcs.txt file in ${setuparea}/info"
echo " 2) rerun the daisyGenDdcs.sh script to update all DDC subdirectories"
echo " 3) change the variables (if any) in daisyProjSetupAdd.sh in ${setuparea}/bin"
echo " 4) setup a working directory with daisySetupProj.sh"
echo ' 5) start Cadence and run (loadi "daisy/skill/daisyCreateDdcLibs.il")'
echo " 6) run daisyBuildCdsLib to build up the new ${setuparea}/cds(oa)/cds.lib"
echo " 7) run daisyOpen (after sourcing project rc file) to set group permissions on everything"

# vim: ft=csh

