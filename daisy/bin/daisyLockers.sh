#!/bin/sh

# Script to find the bad guys ...
#
# JJW, with some help from Thomas for sanity check.
#

find $PROJAREA -type f -printf "%u :: %g :: %p\n" | grep -v $GROUP | grep -v es_usr
find $PROJAREA -type f -printf "%u@student.liu.se\n" | grep -v $GROUP | grep -v es_usr | sort -u
