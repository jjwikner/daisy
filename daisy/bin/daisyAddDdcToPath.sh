#!/bin/bash

# JJ Wikner

# Script to add all the subproject bin paths to the main path

# Parse the file containing staff

PATHx=""

while read ddcName
do
  PATHx=${PATHx}:${PROJAREA}/${ddcName}/bin
  echo "=================================================="
  echo $PATHx
  echo ${PATHx} > $PROJAREA/daisyProjSetup/info/daisyPaths.txt
done < ${PROJAREA}/daisyProjSetup/info/daisyDdcs.txt

echo ${PATHx} > $PROJAREA/daisyProjSetup/info/daisyPaths.txt

