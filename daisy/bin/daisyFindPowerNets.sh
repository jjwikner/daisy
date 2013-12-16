#!/bin/sh

# An example of how to parse through (e.g.) a cdl netlist 
# in order to find certain fixed tokens (vdd, vss, etc.) 
# to isolate all nets associated with power, or anything

# Daisy JJW, 2013-01-11

echo "daisy --> "$0

cdlName=$1

token="vdd"

cat $cdlName | grep -i $token | sed 's/ /\n/g' | grep -i $token | sed 's/:B//' | sed 's/:I//' | sed 's/:O//' | sort -u  > $cdlName.supplies

token="vpwr"

cat $cdlName | grep -i $token | sed 's/ /\n/g' | grep -i $token | sed 's/:B//' | sed 's/:I//' | sed 's/:O//' | sort -u  >> $cdlName.supplies


token="vss"

cat $cdlName | grep -i $token | sed 's/ /\n/g' | grep -i $token | sed 's/:B//' | sed 's/:I//' | sed 's/:O//' | sort -u > $cdlName.grounds

token="gnd"

cat $cdlName | grep -i $token | sed 's/ /\n/g' | grep -i $token | sed 's/:B//' | sed 's/:I//' | sed 's/:O//' | sort -u  >> $cdlName.grounds

echo "daisy <-- "$0
