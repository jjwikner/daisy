#!/bin/sh

cellName=$1

# Remove all X's and replace the <> with []

cat $cellName.spef | sed 's/X//g' | sed 's/</\[/g' | sed 's/>/\]/g'> $cellName.spef.tmp

mv -f $cellName.spef.tmp $cellName.spef



