#!/bin/bash

#
# daisyConvertFig.sh
#
# Takes an arbitrary (?) picture and converts it into a neat 
# format to be used in A4-portrait documents, e.g., 
# openoffice/libreoffice.
#
# ImageMagick is required to have this up and running, as well 
# as bc.
#
# The function could be extended to also take a list of input 
# files. And one could of course make it more user friendly 
# with flags to control the paper width, resolution, etc.
#
# 2013-06-26
# A, JJ Wikner, jjwikner@gmail.com

# ==========================================================
# ==========================================================

funcName=`basename $0`
echo "Executing ${funcName} ... " 

if [ $# -lt 1 ]
then
  echo "    You should specify a file name '$funcName {arg}'!"
  echo "... leaving ${funcName}." 
  exit 1
fi

# Strip the file into its components for pretty-printing, etc.
# Might not be needed as such. 

fileName=$1
baseName="${fileName##*/}"  
pathName="${fileName%/*}"

# Then the parameters. 
PAPERWIDTH=14 # Width in cm (gasp, Europeans! ththth)
DENSITY=72    # Chosen resolution (DPI)

# Find the values required for the convert function
PAPERINCH=`echo "${PAPERWIDTH} / 2.541" | bc -l`
WPIX=`echo "${PAPERINCH} * ${DENSITY}" | bc`
echo "    ${funcName} --> Number of horizontal pixels: $WPIX"

# Then execute the convert function.
# Notice that the width of the picture is only taken into 
# account. 
# We also make sure to only reduce the size of pictures 
# and not increase them in case they were smaller than 
# 14 cm from the beginning. The "\>" flag is handling this.

# The file is sent to the tmp area and inherits most of 
# the basename of the input file name.

convert \
    "${fileName}" \
    -bordercolor white \
    -density ${DENSITY} \
    -resize ${WPIX}x\> \
    "/tmp/${USER}/${baseName}.jpeg"

    
echo "... leaving ${funcName}." 

# ==========================================================
# ==========================================================
