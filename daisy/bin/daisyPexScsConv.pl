#!/bin/csh

# daisyPexScsConv.pl

perl -p -i -e 's/\$\w*=[0-9]*//g' $1 
perl -p -i -e 's/\$\w*//g' $1 

