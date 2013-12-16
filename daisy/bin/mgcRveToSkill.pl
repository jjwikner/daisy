#!/usr/bin/perl
# jjwikner
# mgcRveToSkill.pl

sub atoi {
    my $t;
    foreach my $d (split(//, shift())) {
	$t = $t*10+$d;
    }
    return $t;
}


## Original spef file
$origQueryFile=shift;
$newSkillFile=$origQueryFile . ".il";

# Translation
@exprHash{"mr"} = "(dbCreateRect cellView (list \"text\" \"drawing\") (list ";
@exprHash{"mp"} = "(dbCreatePolygon cellView (list \"text\" \"drawing\") (list ";

## Open second file to convert from query_results to skill
open (QUERYFILEIN, $origQueryFile)  or die "Can't open second file.\n";;
open (SKILLFILEUT, ">$newSkillFile")  or die "Can't open output file.\n";;

while ( $INline = <QUERYFILEIN> ) {
##    print "$INline\n";
    @arrayLine = split(" ", $INline);   
    if (@exprHash{@arrayLine[0]} ) {
	if (@arrayLine[0] eq "mr" ) {
	    ## Create a rectangle
	    $iteration = 2;
	    $itrStart = 1;
	}
	else {
	    ## Now it is a polygon
	    $iteration = @arrayLine[1]; 
	    $itrStart = 0;
	    ##$iteration = 4;
	}	    
	print SKILLFILEUT  @exprHash{@arrayLine[0]} . " ";
	
	 for($itr=1; $itr<=$iteration; $itr++) {
	    print SKILLFILEUT "(list ";
	    print SKILLFILEUT @arrayLine[$itr*2-$itrStart] . " ";
	    print SKILLFILEUT @arrayLine[$itr*2+1-$itrStart] . " ";
	    print SKILLFILEUT " ) " ;		    
	}
	print SKILLFILEUT " ) ) \n ";
    }
    chomp $INline;
}
close (SKILLFILEUT);
close(QUERYFILEIN);
