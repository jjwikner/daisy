#!/bin/csh -f

#setenv libName "smartDustAdcLs"
#setenv ddcName "smartDustAdcLs"
#setenv cdsName "cds.lib"

set ddcName=$1
set libName=$2

set ddcCdsPath=$PROJAREA/$ddcName/cds/
set ddcOaPath=$PROJAREA/$ddcName/oa/

if ($# < 3) then
    set cdsName="cds.lib"
else
    set cdsName=$3
endif

if ($# >= 2) then 

mkdir -p $ddcOaPath
cd $ddcOaPath
cat $DAISYAREA/pdkSpecific/$CDSPROCESSNAME/oa/tech.lib >> $ddcOaPath/cds.lib
cat $DAISYAREA/pdkSpecific/$CDSPROCESSNAME/cds/tech.lib >> $ddcCdsPath/$cdsName

#echo "DEFINE cmos065 /sw/mentor/libraries/cmos065RF_v.522/DK_cmos065lpgp_RF_7m4x0y2z_2V51V8_5.2.2/DATA/LIB/lib/OpenAccess/cmos065" >> cds.lib
#echo "DEFINE cmos065 /sw/mentor/libraries/cmos065RF_v.522/DK_cmos065lpgp_RF_7m4x0y2z_2V51V8_5.2.2/DATA/LIB/lib/cmos065" >> $ddcCdsPath/$cdsName

cdb2oa  -ignorelocks -nodm -lib $libName  -cdslibpath   $ddcCdsPath/$cdsName  
	
sed '$d' $ddcCdsPath/$cdsName >! $ddcCdsPath/_tmp.$cdsName
mv $ddcCdsPath/_tmp.$cdsName  $ddcCdsPath/$cdsName

sed '$d' $ddcOaPath/cds.lib >! $ddcOaPath/_tmp.cds.lib
mv $ddcOaPath/_tmp.cds.lib  $ddcOaPath/cds.lib

sed '$d' $ddcOaPath/cds.lib >! $ddcOaPath/_tmp.cds.lib
mv $ddcOaPath/_tmp.cds.lib  $ddcOaPath/cds.lib

cat $ddcCdsPath/$cdsName >>  $ddcOaPath/$cdsName

sort -u $ddcOaPath/cds.lib > $ddcOaPath/_tmp.cds.lib
mv $ddcOaPath/_tmp.cds.lib  $ddcOaPath/cds.lib

sort -u $ddcOaPath/$cdsName > $ddcOaPath/_tmp.cds.lib
mv $ddcOaPath/_tmp.cds.lib  $ddcOaPath/$cdsName

else
    echo "Insufficient input arguments!"
    echo "Cat this file and check syntax, please!"
    echo "  daisyCds2oa.sh ddcName libName <cds.lib>"
endif
