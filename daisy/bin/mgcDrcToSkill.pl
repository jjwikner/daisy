#!/usr/bin/perl
# jjwikner
# mgcDrcToSkill.pl

sub atoi {
    my $t;
    foreach my $d (split(//, shift())) {
	$t = $t*10+$d;
    }
    return $t;
}


## Original spef file
$origDrcResFile=shift;
$newSkillFile=$origDrcResFile . ".il";

# Translation
@exprHash{"p"} = "(dbCreatePolygon cv drcErrLpp (list";

## Open second file to convert from query_results to skill
open (DRCRESFILEIN, $origDrcResFile)  or die "Can't open second file.\n";;
open (SKILLFILEOUT, ">$newSkillFile")  or die "Can't open output file.\n";;

print SKILLFILEOUT "(setq drcErrLpp (list \"text\" \"drawing\" ) ) \n";
print SKILLFILEOUT "(setq cv cellView)\n";

while ( $INline = <DRCRESFILEIN> ) {

    @arrayLine = split(" ", $INline);   

    if (@exprHash{@arrayLine[0]} ) {
	## Now it is a polygon
	$errNumber = @arrayLine[1]; 
	$noPoints = @arrayLine[2];

	print SKILLFILEOUT  @exprHash{@arrayLine[0]};
	for($itr=1; $itr<=$noPoints; $itr++) {
	    chomp $INline;
	    $INline = <DRCRESFILEIN>;
	    @arrayLine = split(" ", $INline); 
	    print SKILLFILEOUT " (list " . @arrayLine[0]/1000.00 . " " . @arrayLine[1]/1000.00 . ")";
	}
	print SKILLFILEOUT "))\n";
    }
    chomp $INline;
}
close(SKILLFILEOUT);
close(DRCRESFILEIN);
