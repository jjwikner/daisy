#!/bin/tcsh -f

# Project setup script.
# $DAISYAREA, $PROJAREA and $WORKAREA should already be set

## Change these to appropriate values
set CDSDATABASE="oa" # "oa" or "cds"
set PDK_HOME=${DAISYAREA}/pdkSpecific/ams035
######

# Create the correct simrc for the project depending on Cadence version
# and link it into the work area.
if ( ! -e ${PROJAREA}/daisyProjSetup/sim/simrc_${CDSDATABASE} ) then
  cp ${PDK_HOME}/sim/simrc_${CDSDATABASE} ${PROJAREA}/daisyProjSetup/sim/simrc_${CDSDATABASE}
endif
ln -sf ${PROJAREA}/daisyProjSetup/sim/simrc_${CDSDATABASE} ${WORKAREA}/.simrc

# Concatenate the daisy and AMS data.reg file into daisyProjectSetup
# if it doesn't exist there and relink it into the work area.
if ( ! -e ${PROJAREA}/daisyProjSetup/data.reg ) then
  cat ${PDK_HOME}/data.reg ${DAISYAREA}/data.reg > ${PROJAREA}/daisyProjSetup/data.reg
endif
ln -sf ${PROJAREA}/daisyProjSetup/data.reg ${WORKAREA}/data.reg

# Link PDK specific Calibre cellmap file.
ln -sf ${PDK_HOME}/pv/calview.cellmap ${WORKAREA}/calview.cellmap

# Create the modified Calibre runset file to get the rules that work
if ( ! -e ${WORKAREA}/.calibreRunset ) then
  echo "*drcRulesFile: ${PDK_HOME}/pv/calibre/c35b4c3.rules" >! ${WORKAREA}/.calibreRunset
  echo "*lvsRulesFile: ${PDK_HOME}/pv/calibre/c35b4c3.rules" >> ${WORKAREA}/.calibreRunset
  echo "*pexRulesFile: ${PDK_HOME}/pv/calibre/c35b4c3.rules" >> ${WORKAREA}/.calibreRunset
endif

# Make sure the Calibre runset file is not overwritten by AMS's internal 
# process selection function.
if ( ! -e ${WORKAREA}/.amsenv ) then
  echo "exactProcessOption C35B4C3" >! ${WORKAREA}/.amsenv
endif

# Copy and link Assura files.
cp ${PDK_HOME}/pv/assuraUI.prf ${WORKAREA}/.assuraUI.prf
ln -sf ${PDK_HOME}/pv/assura_tech.lib ${WORKAREA}/assura_tech.lib

# Copy local streamout template file
if ( -e ${PDK_HOME}/pv/xstreamOut_${CDSDATABASE} ) then
  if ( ! -e ${WORKAREA}/.xstreamOut ) then
    cp ${PDK_HOME}/pv/xstreamOut_${CDSDATABASE} ${WORKAREA}/.xstreamOut
    chmod 440 ${WORKAREA}/.xstreamOut
  endif
endif

# Copy the layer sets (if any)
if ( -d ${PDK_HOME}/${CDSDATABASE}/layerSets ) then
  if ( ! -d ${WORKAREA}/.cadence/dfII/layerSets ) then
    mkdir -p ${WORKAREA}/.cadence/dfII
    cp -r ${PDK_HOME}/${CDSDATABASE}/layerSets ${WORKAREA}/.cadence/dfII
    chmod -R a+rwX .cadence
  endif
endif

# Copy the job policies (if any)
if ( -d ${PDK_HOME}/${CDSDATABASE}/jobpolicy ) then
  if ( ! -d ${WORKAREA}/.cadence/jobpolicy ) then
    mkdir -p ${WORKAREA}/.cadence
    cp -r ${PDK_HOME}/${CDSDATABASE}/jobpolicy ${WORKAREA}/.cadence
    chmod -R a+rwX .cadence
  endif
endif

# vim: ft=tcsh
