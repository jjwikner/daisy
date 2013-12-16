#!/bin/sh

cdlName=$1

token="vdd"

cat $cdlName | grep -i $token | sed 's/ /\n/g' | grep -i $token | sed 's/:B//' | sed 's/:I//' | sed 's/:O//' | sort -u  > $cdlName.supplies

token="vpwr"

cat $cdlName | grep -i $token | sed 's/ /\n/g' | grep -i $token | sed 's/:B//' | sed 's/:I//' | sed 's/:O//' | sort -u  >> $cdlName.supplies


token="vss"

cat $cdlName | grep -i $token | sed 's/ /\n/g' | grep -i $token | sed 's/:B//' | sed 's/:I//' | sed 's/:O//' | sort -u > $cdlName.grounds

token="gnd"

cat $cdlName | grep -i $token | sed 's/ /\n/g' | grep -i $token | sed 's/:B//' | sed 's/:I//' | sed 's/:O//' | sort -u  >> $cdlName.grounds



