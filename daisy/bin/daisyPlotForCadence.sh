#!/bin/csh 

# Helper script for the cadence plot generator. 
# Some tweaks were needed due to several versions of software and systems.

#module rm local
#module add localfirst

#setenv PATH "/sw/local5/bin:/sw/local/bin:/bin:/usr/bin:/usr/X11R6/bin:${DAISYAREA}/bin/"
unsetenv LD_LIBRARY_PATH
set fileName = $1
set angle  = $2 

ps2epsi $fileName.eps $fileName.epsi
convert -rotate $angle $fileName.eps $fileName.tiff
convert -rotate $angle $fileName.eps $fileName.png

convert $fileName.eps $fileName.pdf
# /sw/local5/bin/ps2pdf $fileName.eps $fileName.pdf
