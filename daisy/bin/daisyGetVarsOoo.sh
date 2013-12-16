#!/bin/sh 

# Given an ooo file, fileName.odt as an input argument, 
# this script outputs the Document revision
# The output can be changed to any other variables by changing the KEY variable
# the output path is also as an (optional) input argument
#
# Example usage of this snippet: 
# daisyGetRev.sh /proj/es/ES_Documents/weekly/doc_P2B.odt $HOME/Desktop/
#
#
# (c) M Reza Sadeghifar, ES -- ISY -- Linköping
#
# Change history
#
# Also removed the file writer and added an additional input argument.
# Sometimes the variables are stored in the meta.xml - don't know why. 
# Perhaps dependent on OOO version ? JJW 2011-11-03 
#
# Extended by JJW, 2011-11-03 to get all variables.
#
# 2011-10-24 First release
#
oooFile=$1

KEY="Document repo"
docRepo=`daisyGetVarOoo.sh $oooFile "$KEY"`
KEY="Document number"
docNo=`daisyGetVarOoo.sh $oooFile "$KEY"`
KEY="Document type"
docType=`daisyGetVarOoo.sh $oooFile "$KEY"`
KEY="Document tag"
docTag=`daisyGetVarOoo.sh $oooFile "$KEY"`
KEY="Document revision"
docRev=`daisyGetVarOoo.sh $oooFile "$KEY"`
echo "${docRepo}_${docNo}_${docType}_${docTag}_${docRev}"
# This can be used to update the revision too.
