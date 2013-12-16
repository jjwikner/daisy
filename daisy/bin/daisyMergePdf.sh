#!/bin/env tcsh

# This script merges pdf files into a file called allPDFs.pdf
# Any number of arguments can be sent to the script
#
# Examples
#
# %% Merging three pdf files %%
#     
# daisyMergePdf file1.pdf file2.pdf file3.pdf 
# 
# %% Using regular search patterns %%
#
# daisyMergePdf TEK*.pdf 
# 
# daisyMergePdf `find * | grep pdf`  
#
#
# (c) Niklas U Andersson, Electronics Systems, Linköping University
#
# Change History
#
# 2010-12-09 First Release

echo 'The following .pdf files will be merged: $*'

echo "Hello World"
# Creating empty temporary files
touch tmp1PDF.pdf
touch tmp2PDF.pdf

# Loop through list of arguments (files) sent to function
# and merge all these into one single file named "allPDFs.pdf"
 
foreach file ($*)
   echo Merging "$file"
   gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=tmp2PDF.pdf tmp1PDF.pdf $file
   cp tmp2PDF.pdf tmp1PDF.pdf
   \rm -f tmp2PDF.pdf
end

mv tmp1PDF.pdf allPDFs.pdf
