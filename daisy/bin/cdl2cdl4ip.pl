#! /usr/bin/perl

#=============================================================
#Name: cdl2cdl4ip.pl
#Renamed: netlistTranslation.pl
#
#Version History
# 1.0    Created by grbr
# 1.0.1  Changed name and added to common bin directory
#
#Author:  [grbr] Gregory.Brillant@siconsemi.com 
#Modified: [jjw] J Jacob Wikner (jacob.wikner@siconsemi.com)
#=============================================================

#=============================================================
#                            DESCRIPTION
#=============================================================
#This script is used to modify the syntax of a netlist:
#	-INCLUDE files are copied in the new netlist
#	-a tag is applied to the name of the different cells, 
#exept for the top cell, and the cells present in the INCLUDE
#files 
#	-double spaces are removed
#	-line break are removed
#	-Brackets <> are changed to []
#
#Usage:	netlistTranslation.pl netlist_name tag_name topCell_name
#Return:	netlist_name_modified
#
#Default values:
#	-tag: cellTopName
#	-top cell: cellTopName and cellTopName2

#=============================================================
#                            SETUP
#=============================================================
$start=time();
$temps=localtime(time());
print "\nStart: $temps \n";

#Arguments
$netlist=$ARGV[0]; #Netlist name
$daisyTag=$ARGV[1]; #tag added before cells name
$topCell=$ARGV[2]; #top cell name

#Variables
$line;
@cellToSkip;

#Arguments checking and empty fields setup
if(!$daisyTag || $daisyTag eq "default"){
	$daisyTag="cellTopName";
}
	
#Arguments display
print "Netlist being proceeded: $netlist\n";
print "Sicon tag: $daisyTag\n";	
	
#Open netlist and source_added files
open(FILE, "$netlist") or die ("problem encountered: impossible to open $netlist \n usage: perl netlistTranslation.pl netlist_name daisyTag\n");
print "$netlist is opened\n";

#create temp file
open (TEMP, '>tempNetlist.txt') or die ("problem encountered: impossible to create a temp file");
print "creation of temp netlist\n\n";


#=============================================================
#                            FUNCTIONS
#=============================================================


############### ouvrirIncludeFile  #######################
#Role: Open and include files preceded by "INCLUDE" command

$nbreIncludedFiles=0;#numbers of included files
$count=0;#hierarchy within the included files (some files are included within included files)
sub ouvrirIncludeFile{
	$count++;
	$nbreIncludedFiles++;
	$fileName=$_[0];
	
	#check if the path is absolute or relative
	if($fileName=~m/\/home\//){
		$fileName=~m/\/(.*)\//;
		$directory="/".$1."/";#Save the absolute path
		print "INCLUDE files directory: $directory \n";
	}
	#if path is realtive => absolute
	else{
		$fileName="$directory"."$fileName";
	}
	open(SOURCE, "$fileName") or die ("problem encountered: impossible to open $fileName \n");
	print "$fileName is opened. $nbreIncludedFiles file(s) included. \n";
	@{"includeFile$count"}=<SOURCE>;
	close(SOURCE);
	${"indice$count"}=0;
	while($line=${"includeFile$count"}[${"indice$count"}]){
		${"indice$count"}++;
		if($line=~m/\.INCLUDE *(.*?)\n/){
			ouvrirIncludeFile($1);
		}
		if($line=~m/\.SUBCKT *(.*?)\s/){
			push(@cellToSkip,$1);
		}			
		print TEMP "$line";
	}
	$count--;
	}

######################## End  #############################




#####################  putTag  ############################
#Role: Put daisyTag_ before the different cells name
sub putTag{
	if($line=~m/\.SUBCKT/){ #reduce execution time
		foreach $cell (@cellToSkip) {
			if($line=~m/\.SUBCKT\s*$cell/ && $cell=~m/\w/){
			goto FIN;
			}
		}
	$line=~s/\.SUBCKT /.SUBCKT $daisyTag\_/;
	FIN:
	}
}	

######################## End  #############################




#####################  putTag2  ############################
#Role:Change " / " by " / daisyTag_"	
sub putTag2{
	if($line=~m/ \/ /){ #reduce execution time
		foreach $cell (@cellToSkip) {
			if($line=~m/\/\s*$cell/ && $cell=~m/\w/){
			goto FIN;
			}
		}
	$line=~s/ \/ / \/ $daisyTag\_/;
	FIN:
	}
}	

######################## End  #############################




#####################  changeBra  ############################
#Role:Change "<>" by "[]"
sub changeBra{
	while($line=~m/</ || $line=~m/>/){	
			$line=~s/</[/;
			$line=~s/>/]/;
	}
}	

######################## End  #############################


#=============================================================
#                            BODY
#=============================================================

push(@cellToSkip,$daisyTag,"cellTopName","cellTopName2",$topCell);

#Read the netlist line by line
while($line=<FILE>){
#Check for files to include	
	if($line=~m/\.INCLUDE(.*?)\n/){
		ouvrirIncludeFile($1);
		print "\nThe following cells will not receive the tag: @cellToSkip\n\n";
	}

#Put tag
	putTag($line);
	
#Replace "  " by " "	
	while($line=~m/  /){
		$line=~s/  / /;
	}
	

#Remove the line break "\n" => 2 lines become 1
	if ($line=~m/ \/ \n/){
		$line=~s/ \/ \n/ \/ /;
		changeBra($line);
		putTag2($line);
		print TEMP "$line";#print the first part of the line in TEMP	
		$line=<FILE>;#read the second part of the line
		$line=~s/\+ //;#Remove the "+" in the new line ("+" is present in line l+1 before the removing of "\n")
	}
		
#put tag	
	putTag2($line);
	
#change braquets
	changeBra($line);
	
#Print each line in the temp file
	print TEMP "$line";
}



#=============================================================
#                    FILE MANIPULATION
#=============================================================

print "file manipulation:start\n";
close(TEMP);
close(FILE);
close(SOURCE);
#unlink "$netlist";
rename "tempNetlist.txt","$netlist"."_modified";
unlink "tempNetlist.txt";
print "file manipulation:end\n";
$end=time();
$execTime=$end-$start;
$min=int($execTime/60);
$sec=$execTime%60;
print "execution time: $min min $sec sec\n\n"
