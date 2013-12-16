#!/bin/bash

# JJ Wikner

# Script to generate a combined ics file for a 
# group of people at linkoping university. In 
# addition to the ics an html page is generated 
# for the intranet.

# The
# main university timeedit database is used for
# reference:

# Parse the file containing staff

a=""
while read ddc
do
    a="${a}:\$PROJAREA/${ddc}/$1"
done < ${PROJAREA}/daisyProjSetup/info/daisyDdcs.txt

echo $a
