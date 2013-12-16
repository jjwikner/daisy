#!/bin/sh

# Here we could add more, for example if it testlib, etc.

while test "$1" != "" ; do
    echo "DEFINE $1 $1" >> cds.lib
    shift
done
