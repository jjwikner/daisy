#!/usr/bin/perl

##############################################################
#
# Name:   copyCadenceDB.pl
# Author: niklas.andersson@siconsemi.com
#
##############################################################
#                       DESCRIPTION
##############################################################
# 
# Copies a complete Cadence database, e.g., for macro release.
# After copy the library structure is parsed and necessary cds.lib
# files are created.
# The file needs an input file fron the following skill command.
#
# $DAISYAREA/skill/daisyCopyHierPrep.il
# 
# Usage: copyCadenceDB.pl <INPUTFILE> <MACRODESTINATION>
#        e.g., 
# ./daisyCopyCdsDb.pl daisyTop.daisyTopTop.hier.txt $IPAREA/AFE/advance2/daisy/tsmc/tsmcN90/pre20080924
# 
##############################################################
#                      CHANGE HISTORY
##############################################################
# 
# 2011-10-21: Renamed, v1.1
# 2008-09-26: First release, v1.0
#
##############################################################

# Input path file
$netlist=$ARGV[0];
open(FILE_IN, "$netlist");

# Destination library for the macro
$macroPath=$ARGV[1];

# Special views to be copied in addition to those in the input path file 
# Daisy IP list
# @specialViews = ("schematicLVS","verilogams","veriloga","layoutCOK","layoutROK","schematicVerilog","functional");
# Zoran AFE IP list
@specialViews = ("schematicLVS");

# Library tags and property files for Cadence libraries   
@libTagArray =("cdsinfo.tag","prop.xx");

# Start the copying

while(<FILE_IN>)
{
    
    # Extract project name
    $firstLine = $_;
    @firstLineArray = split('/',$firstLine);
    $projectName = @firstLineArray[3];
    
    
    # Split the path in clever units to glue together futher down in the script
    $originalPath = $_;
    $originalPath =~ s/\n//;
        
    s/$projectName\//$projectName\/ \//;
    s/(\w+[a-z.]\w+\n)/ $1/; 
    #s/(\w+\/\s\w+)/ \/$1/;
    s/([\w\#]+\/\s\w+)/ \/$1/;
    
    @lineASrc = split(' ',$_);
    
    s/\/cdsN90\//\/cds\//;
    @lineADest = split(' ',$_);
    
    system "mkdir","-p","$macroPath/cds/@lineADest[1]@lineADest[2]";
    system "cp","-rp","@lineASrc[0]@lineASrc[1]@lineASrc[2]@lineASrc[3]","$macroPath/cds/@lineADest[1]@lineADest[2]";
    
    # Copy special cell views
    foreach $view (@specialViews)
    {
	system "cp","-rp","@lineASrc[0]@lineASrc[1]@lineASrc[2]/$view","$macroPath/cds/@lineADest[1]@lineADest[2]";	
    }
    
    # Copy special library tags and prop.xx
    foreach $tag (@libTagArray)
    {
	system "cp","-rp","@lineASrc[0]@lineASrc[1]$tag","$macroPath/cds/@lineADest[1]";
    }

    
} # end while


# PARSE LIBRARY STRUCTURE AND CREATE THE CDL.LIB FILES NEEDED

# Root library cds.lib is created
open(ROOT_FILE_OUT,">$macroPath/cds/cds.lib");

# read out library content
opendir(DIR,"$macroPath/cds");
@rootLibArray = readdir(DIR); 
closedir(DIR);

# parse and create the root cds.lib file

foreach $item (@rootLibArray)
{
    $_ = $item;
  
    # check if cds.lib or . or .. 
    if  (m/cds.lib|\./)
    {} 
    else
    { 
	print ROOT_FILE_OUT "INCLUDE $item/cds/cds.lib\n";
    }
    
}

close(ROOT_FILE_OUT);

# find all subdirectories that are being "INCLUDED" in the root cds.lib
opendir(DIR,"$macroPath/cds");
@subLibArray=`find $macroPath/cds -maxdepth 1 -mindepth 1 -type d -type d -printf "%f\n"`;

foreach $subLib (@subLibArray)
{

    # create new sub cds.lib file
    $subLib =~ s/\n//;
    open(FILE,">$macroPath/cds/$subLib/cds/cds.lib");
    
    # find sub sub libs and print "DEFINE subSubLib subSubLib" in local cds.lib file
    @subSubLibArray=`find $macroPath/cds/$subLib/cds -maxdepth 1 -mindepth 1 -type d -type d -printf "%f\n"`;
    foreach $subSubLib (@subSubLibArray)
    {
	$subSubLib =~ s/\n//;
	print FILE "DEFINE $subSubLib $subSubLib\n";
    }
    
    close(FILE);	 
    print("New file Created:"."$macropPath/cds/$subLib/cds/cds.lib\n");

}



# chmod -R +w AFEDIR/ ; rm -fr AFEDIR/ ; ./daisyCopyCdsDb.pl daisyAFE.daisyAFETop.hier.txt ./AFEDIR/;


