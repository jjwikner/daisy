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
# 2011-10-24 First release
#



if [ $# -eq 2 ]
then
    oooFile=$1
#    oooPath=$2
    KEY=$2
elif [ $# -eq 1 ]
then
    oooFile=$1
#    oooPath="/tmp/"
    KEY="Document revision"
else
    echo "DAISY:: Incorrect number of input arguments."
    exit 1   # exit the script
fi

# KEY="Document revision"

#echo $KEY
#echo $oooPath
#echo $oooFile
 
#cp $oooFile $oooPath/$oooFile
#cd $oooPath
unzip -pa $oooFile meta.xml | tr -s "<" "\n" | grep "$KEY" | tr -s ">" "\n" | tail -n 1 
#> $oooPath/${oooFile}.rev
#cat $oooPath/${oooFile}.rev

