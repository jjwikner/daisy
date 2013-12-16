#!/bin/sh 

# 

# the path is an (optional) input argument
#
# Example usage of this snippet: 
# daisyPlotForMatlab.sh 
#
#
# (c) M Reza Sadeghifar, ES -- ISY -- Linköping
#
# Change hisotry
#
# 2011-10-24 First release
#

if [ $# -lt 1 ]
then
    # Assume default path
    pathh=$USERAREA/doc/mfigs/
    #pathh=./tbshell
else
    # use the input path
    pathh=$1
fi

# echo $pathh
cd $pathh

trap 'rm .newer' 0
touch .newer
while true;do
    find . -maxdepth 1 -newer .newer -type  f -name '*.eps' -exec ps2epsi {} {}.epsi \;
    find . -maxdepth 1 -newer .newer -type  f -name '*.eps' -exec convert {} {}.tiff \;
    find . -maxdepth 1 -newer .newer -type  f -name '*.eps' -exec convert {} {}.png \;
    find . -maxdepth 1 -newer .newer -type  f -name '*.eps' -exec convert {} {}.pdf \;
    #(find . -name "*.eps" |tr -s ".eps" "\n"| tr -s "/" "\n" |tail -n 1)
    touch .newer
    sleep 10
done

