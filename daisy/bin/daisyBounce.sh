#!/bin/sh
# Echoes a file from the file system to the linux domain.
#
# Updated with comments and renamed, JJW 2011-11-09

echo "DAISY:: Bounced $1 to e-mail account" | mail -s "Bounced $1 to e-mail account" -a $1 $EMAIL
