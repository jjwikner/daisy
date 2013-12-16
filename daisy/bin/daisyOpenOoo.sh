#!/bin/csh 
unsetenv LD_LIBRARY_PATH
set fileName = $1
openoffice.org $fileName &
