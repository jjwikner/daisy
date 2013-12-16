#!/bin/sh

cellName=$1

# Pick up the latest file from the verification directory
cp  $WORKAREA/verification/$cellName/xrc/$cellName.pex.netlist .

# Search for all transistors in the pex file
cat $cellName.pex.netlist | grep -i "mm" | tr " " "\n" | sort -u | grep -i "mm" | sed 's/\\//g' > $cellName.nets.spef

# Search for all "illegal" back-slashes and remove them
cat $cellName.pex.netlist | sed 's/\\//g'  > $cellName.pex.netlist.tmp
mv -f $cellName.pex.netlist.tmp $cellName.pex.netlist



