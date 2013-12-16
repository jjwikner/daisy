#!/bin/bash

# This snippet is a help function for the cadence cad environment.
# It will look through lock files in the cadence database and 
# check who the lucky guy is...
#
# 
#

read -r -a Words <<< `cat "$1" | grep LoginName`
echo "(setq LOCKUSER '${Words[1]})" > /tmp/daisyLockUser.il

