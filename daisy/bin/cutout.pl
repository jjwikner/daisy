#!/usr/local/bin/perl -w -X

# cutout.pl -- useful for the cypress eldo flow to find monte carlo results.


$inFileName = $ARGV[0];
## Possible be an input argument. 
$numberOfBins = 33 ;
##$numberOfBins = $ARGV[1];

open(INPUT_FILE, $inFileName) || die("Could not open input file");
$end = $numberOfBins + 8 ;
$counter = $end + 1;
$fileIsOpened = 0;

# open(OUTPUT_FILE, $outFileName) || die("Could not open output file");
# close(OUTPUT_FILE);

while($line = <INPUT_FILE>) {
    if($counter < $end) {
	if($line =~ /.*Distribution of */) {
	    close(OUTPUT_FILE);
	    $counter = 0;
	    $fileIsOpened = 1;
	    $outFileName = "> " . $inFileName . "." . substr($line, 16);
	    open(OUTPUT_FILE, $outFileName) || die("Could not open output file");	    
	}
	else {	    
	    printf OUTPUT_FILE $line;
	}
	$counter = $counter + 1;
    }
    elsif($fileIsOpened == 1) {
	close(OUTPUT_FILE);
	$fileIsOpened = 0;
   }
    
    if($line =~ /.*Distribution of */) {
	if($fileIsOpened == 0) { 
	    $outFileName = "> " . $inFileName . "." . substr($line, 16);
	    open(OUTPUT_FILE, $outFileName) || die("Could not open output file");
	    $fileIsOpened = 1;
	}	
	printf OUTPUT_FILE $line;
	$counter = 0;
    }
}

close(INPUT_FILE);
if($fileIsOpened = 1) {
    close(OUTPUT_FILE)
    }
