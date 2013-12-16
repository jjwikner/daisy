#!/usr/bin/perl
# jjwikner
# cdl2hcell.pl

## Original cdl file (typically the file coming out of lvs, i.e., .src.net)
## Assume it to be first input argument
$cdlFile=shift;
## Assume hcell file to be second input argument
$hcellFile=shift;

## Open second file to convert from query_results to skill
open (CDLFILE, $cdlFile)  or die "Can't open second file.\n";;
open (HCELLFILE, ">$hcellFile")  or die "Can't open output file.\n";;

print HCELLFILE "// CALIBRE hcell file \n";

while ( $INline = <CDLFILE> ) {

    @arrayLine = split(" ", $INline);   

    if (@arrayLine[0] eq ".SUBCKT")  {
	print HCELLFILE  @arrayLine[1] . " " .  @arrayLine[1] . "\n";
    }
}

close(HCELLFILE);
close(CDLFILE);
